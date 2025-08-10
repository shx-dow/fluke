/* includes */

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#include <dirent.h>
#include <limits.h>
#include <sys/stat.h>

/* defines */

#define FLUKE_VERSION "0.0.1"
#define FLUKE_TAB_STOP 8
#define FLUKE_QUIT_TIMES 3
#define FLUKE_LINE_NUMBER_WIDTH 6

#define CTRL_KEY(k) ((k) & 0x1f)

enum editorKey {
  BACKSPACE = 127,
  ARROW_LEFT = 1000,
  ARROW_RIGHT,
  ARROW_UP,
  ARROW_DOWN,
  DEL_KEY,
  HOME_KEY,
  END_KEY,
  PAGE_UP,
  PAGE_DOWN,
  CTRL_ARROW_LEFT,
  CTRL_ARROW_RIGHT
};

enum editorHighlight {
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
};

/* theme system */
typedef struct {
  const char *name;
  int normal;
  int comment;
  int keyword1;
  int keyword2;
  int string;
  int number;
  int match;
  int line_number;
  int status_bg;
  int status_fg;
} Theme;

static const Theme themes[] = {
  {"default", 37, 36, 33, 32, 35, 31, 34, 90, 7, 0},
  {"dark", 37, 36, 93, 92, 95, 91, 94, 90, 7, 0},
  {"light", 30, 34, 31, 32, 35, 33, 36, 90, 47, 30},
  {"monokai", 37, 102, 197, 148, 186, 208, 81, 59, 235, 252},
  {"solarized", 244, 61, 33, 37, 64, 166, 136, 240, 235, 230}
};

static int current_theme = 0;

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/* data */

struct editorSyntax {
  char *filetype;
  char **filematch;
  char **keywords;
  char *singleline_comment_start;
  char *multiline_comment_start;
  char *multiline_comment_end;
  int flags;
};

typedef struct erow {
  int idx;
  int size;
  int rsize;
  char *chars;
  char *render;
  unsigned char *hl;
  int hl_open_comment;
} erow;

/* mark system */
typedef struct {
  int cy, cx;
  int set;
} Mark;

struct editorConfig {
  int cx, cy;
  int rx;
  int rowoff;
  int coloff;
  int screenrows;
  int screencols;
  int numrows;
  erow *row;
  int dirty;
  char *filename;
  char statusmsg[80];
  time_t statusmsg_time;
  struct editorSyntax *syntax;
  struct termios orig_termios;
  int is_insert_mode;
  int soft_wrap_enabled;
  int vrowoff; /* visual row offset when wrapping */
  int show_line_numbers;
  int read_only;
  Mark marks[26]; /* marks a-z */
};

struct editorConfig E;

/* search state */
static char g_search_query[128] = "";

/* forward decls used by undo/redo helpers */
void editorInsertRow(int at, char *s, size_t len);
void editorFreeRow(erow *row);

/* undo/redo */
#define FLUKE_UNDO_MAX 128
typedef struct BufferSnapshot {
  int numrows;
  char **rows;
  int *sizes;
  int cx, cy, rowoff, coloff;
} BufferSnapshot;

static BufferSnapshot *g_undo_stack[FLUKE_UNDO_MAX];
static int g_undo_len = 0;
static BufferSnapshot *g_redo_stack[FLUKE_UNDO_MAX];
static int g_redo_len = 0;

static void freeSnapshot(BufferSnapshot *s) {
  if (!s) return;
  if (s->rows) {
    for (int i = 0; i < s->numrows; i++) free(s->rows[i]);
    free(s->rows);
  }
  free(s->sizes);
  free(s);
}

static BufferSnapshot *takeSnapshot() {
  BufferSnapshot *s = (BufferSnapshot *)calloc(1, sizeof(BufferSnapshot));
  s->numrows = E.numrows;
  s->rows = (char **)calloc((size_t)E.numrows, sizeof(char *));
  s->sizes = (int *)calloc((size_t)E.numrows, sizeof(int));
  for (int i = 0; i < E.numrows; i++) {
    s->sizes[i] = E.row[i].size;
    s->rows[i] = (char *)malloc((size_t)E.row[i].size + 1);
    memcpy(s->rows[i], E.row[i].chars, (size_t)E.row[i].size);
    s->rows[i][E.row[i].size] = '\0';
  }
  s->cx = E.cx; s->cy = E.cy; s->rowoff = E.rowoff; s->coloff = E.coloff;
  return s;
}

static void restoreFromSnapshot(BufferSnapshot *s) {
  /* free current buffer */
  for (int i = 0; i < E.numrows; i++) editorFreeRow(&E.row[i]);
  free(E.row);
  E.row = NULL;
  E.numrows = 0;

  /* rebuild from snapshot */
  for (int i = 0; i < s->numrows; i++) {
    editorInsertRow(E.numrows, s->rows[i], s->sizes[i]);
  }
  E.cx = s->cx; E.cy = s->cy; E.rowoff = s->rowoff; E.coloff = s->coloff;
  E.dirty++;
}

static void clearRedoStack() {
  for (int i = 0; i < g_redo_len; i++) freeSnapshot(g_redo_stack[i]);
  g_redo_len = 0;
}

static void pushUndoSnapshot() {
  if (g_undo_len == FLUKE_UNDO_MAX) {
    freeSnapshot(g_undo_stack[0]);
    memmove(&g_undo_stack[0], &g_undo_stack[1], sizeof(g_undo_stack) - sizeof(g_undo_stack[0]));
    g_undo_len--;
  }
  g_undo_stack[g_undo_len++] = takeSnapshot();
  clearRedoStack();
}

static void undoAction() {
  if (g_undo_len == 0) return;
  /* push current to redo */
  if (g_redo_len == FLUKE_UNDO_MAX) {
    freeSnapshot(g_redo_stack[0]);
    memmove(&g_redo_stack[0], &g_redo_stack[1], sizeof(g_redo_stack) - sizeof(g_redo_stack[0]));
    g_redo_len--;
  }
  g_redo_stack[g_redo_len++] = takeSnapshot();

  BufferSnapshot *s = g_undo_stack[--g_undo_len];
  restoreFromSnapshot(s);
  freeSnapshot(s);
}

static void redoAction() {
  if (g_redo_len == 0) return;
  /* push current to undo */
  if (g_undo_len == FLUKE_UNDO_MAX) {
    freeSnapshot(g_undo_stack[0]);
    memmove(&g_undo_stack[0], &g_undo_stack[1], sizeof(g_undo_stack) - sizeof(g_undo_stack[0]));
    g_undo_len--;
  }
  g_undo_stack[g_undo_len++] = takeSnapshot();

  BufferSnapshot *s = g_redo_stack[--g_redo_len];
  restoreFromSnapshot(s);
  freeSnapshot(s);
}

/* filetypes */

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",

  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", NULL
};

struct editorSyntax HLDB[] = {
  {
    "c",
    C_HL_extensions,
    C_HL_keywords,
    "//", "/*", "*/",
    HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/* prototypes */

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void editorInsertRow(int at, char *s, size_t len);
void editorFreeRow(erow *row);
void editorGotoLine();
void editorCommandPalette();
static void editorToggleWrap();
static void editorToggleLineNumbers();
static void freePalette();
static void loadCommands();
static void updatePaletteScores();
/* append buffer definition */
struct abuf {
  char *b;
  int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len) {
  char *new = realloc(ab->b, ab->len + len);
  if (new == NULL) return;
  memcpy(&new[ab->len], s, len);
  ab->b = new;
  ab->len += len;
}

void abFree(struct abuf *ab) {
  free(ab->b);
}

static void drawCommandPalette(struct abuf *ab);
static void editorQuitNow();
void editorOpenPrompt();
static void persistCursorPositionIfAny();
static void restoreCursorPositionIfAny();
static int fuzzyScore(const char *pattern, const char *candidate);


static void editorClearBuffer();
static void loadConfig();
static void saveConfig();
static void editorAutosave();
static void editorCheckRecovery();
static void editorCleanupAutosave();

/* terminal */

void die(const char *s) {
  if (write(STDOUT_FILENO, "\x1b[2J", 4) == -1) { /* ignore error */ }
  if (write(STDOUT_FILENO, "\x1b[H", 3) == -1) { /* ignore error */ }

  perror(s);
  exit(1);
}

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}

void enableRawMode() {
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
  atexit(disableRawMode);

  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int editorReadKey() {
  int nread;
  char c;
  while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    if (nread == -1 && errno != EAGAIN) die("read");
  }

  if (c == '\x1b') {
    char seq[4];

    if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

    if (seq[0] == '[') {
      if (seq[1] >= '0' && seq[1] <= '9') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        if (seq[2] == '~') {
          switch (seq[1]) {
            case '1': return HOME_KEY;
            case '3': return DEL_KEY;
            case '4': return END_KEY;
            case '5': return PAGE_UP;
            case '6': return PAGE_DOWN;
            case '7': return HOME_KEY;
            case '8': return END_KEY;
          }
        }
      } else if (seq[1] == '1') {
        if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        if (seq[2] == ';') {
          if (read(STDIN_FILENO, &seq[3], 1) != 1) return '\x1b';
          if (seq[3] == '5') {
            /* Ctrl+Arrow sequences - ESC[1;5C or ESC[1;5D */
            char arrow;
            if (read(STDIN_FILENO, &arrow, 1) != 1) return '\x1b';
            switch (arrow) {
              case 'C': return CTRL_ARROW_RIGHT;
              case 'D': return CTRL_ARROW_LEFT;
            }
          } else if (seq[3] == '6') {
            /* Ctrl+Shift+Arrow sequences - ESC[1;6C or ESC[1;6D */
            char arrow;
            if (read(STDIN_FILENO, &arrow, 1) != 1) return '\x1b';
            /* For now, treat Ctrl+Shift+Arrow same as Ctrl+Arrow */
            switch (arrow) {
              case 'C': return CTRL_ARROW_RIGHT;
              case 'D': return CTRL_ARROW_LEFT;
            }
          }
        }
      } else {
        switch (seq[1]) {
          case 'A': return ARROW_UP;
          case 'B': return ARROW_DOWN;
          case 'C': return ARROW_RIGHT;
          case 'D': return ARROW_LEFT;
          case 'H': return HOME_KEY;
          case 'F': return END_KEY;
        }
      }
    } else if (seq[0] == 'O') {
      switch (seq[1]) {
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
      }
    } else if (seq[0] == 'b') {
      /* Some terminals send ESCb for Ctrl+Left (word back) */
      return CTRL_ARROW_LEFT;
    } else if (seq[0] == 'f') {
      /* Some terminals send ESCf for Ctrl+Right (word forward) */
      return CTRL_ARROW_RIGHT;
    } else if (seq[0] == 'O') {
      switch (seq[1]) {
        case 'H': return HOME_KEY;
        case 'F': return END_KEY;
      }
    }

    return '\x1b';
  } else {
    return c;
  }
}

int getCursorPosition(int *rows, int *cols) {
  char buf[32];
  unsigned int i = 0;

  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

  while (i < sizeof(buf) - 1) {
    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
    if (buf[i] == 'R') break;
    i++;
  }
  buf[i] = '\0';

  if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

  return 0;
}

int getWindowSize(int *rows, int *cols) {
  struct winsize ws;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
    return getCursorPosition(rows, cols);
  } else {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

/* syntax highlighting */

int is_separator(int c) {
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
  row->hl = realloc(row->hl, row->rsize);
  memset(row->hl, HL_NORMAL, row->rsize);

  if (E.syntax == NULL) return;

  char **keywords = E.syntax->keywords;

  char *scs = E.syntax->singleline_comment_start;
  char *mcs = E.syntax->multiline_comment_start;
  char *mce = E.syntax->multiline_comment_end;

  int scs_len = scs ? strlen(scs) : 0;
  int mcs_len = mcs ? strlen(mcs) : 0;
  int mce_len = mce ? strlen(mce) : 0;

  int prev_sep = 1;
  int in_string = 0;
  int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

  int i = 0;
  while (i < row->rsize) {
    char c = row->render[i];
    unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

    if (scs_len && !in_string && !in_comment) {
      if (!strncmp(&row->render[i], scs, scs_len)) {
        memset(&row->hl[i], HL_COMMENT, row->rsize - i);
        break;
      }
    }

    if (mcs_len && mce_len && !in_string) {
      if (in_comment) {
        row->hl[i] = HL_MLCOMMENT;
        if (!strncmp(&row->render[i], mce, mce_len)) {
          memset(&row->hl[i], HL_MLCOMMENT, mce_len);
          i += mce_len;
          in_comment = 0;
          prev_sep = 1;
          continue;
        } else {
          i++;
          continue;
        }
      } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
        memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
        i += mcs_len;
        in_comment = 1;
        continue;
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
      if (in_string) {
        row->hl[i] = HL_STRING;
        if (c == '\\' && i + 1 < row->rsize) {
          row->hl[i + 1] = HL_STRING;
          i += 2;
          continue;
        }
        if (c == in_string) in_string = 0;
        i++;
        prev_sep = 1;
        continue;
      } else {
        if (c == '"' || c == '\'') {
          in_string = c;
          row->hl[i] = HL_STRING;
          i++;
          continue;
        }
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
      if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
          (c == '.' && prev_hl == HL_NUMBER)) {
        row->hl[i] = HL_NUMBER;
        i++;
        prev_sep = 0;
        continue;
      }
    }

    if (prev_sep) {
      int j;
      for (j = 0; keywords[j]; j++) {
        int klen = strlen(keywords[j]);
        int kw2 = keywords[j][klen - 1] == '|';
        if (kw2) klen--;

        if (!strncmp(&row->render[i], keywords[j], klen) &&
            is_separator(row->render[i + klen])) {
          memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
          i += klen;
          break;
        }
      }
      if (keywords[j] != NULL) {
        prev_sep = 0;
        continue;
      }
    }

    prev_sep = is_separator(c);
    i++;
  }

  int changed = (row->hl_open_comment != in_comment);
  row->hl_open_comment = in_comment;
  if (changed && row->idx + 1 < E.numrows)
    editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColor(int hl) {
  const Theme *theme = &themes[current_theme];
  switch (hl) {
    case HL_COMMENT:
    case HL_MLCOMMENT: return theme->comment;
    case HL_KEYWORD1: return theme->keyword1;
    case HL_KEYWORD2: return theme->keyword2;
    case HL_STRING: return theme->string;
    case HL_NUMBER: return theme->number;
    case HL_MATCH: return theme->match;
    default: return theme->normal;
  }
}

void editorSelectSyntaxHighlight() {
  E.syntax = NULL;
  if (E.filename == NULL) return;

  char *ext = strrchr(E.filename, '.');

  for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
    struct editorSyntax *s = &HLDB[j];
    unsigned int i = 0;
    while (s->filematch[i]) {
      int is_ext = (s->filematch[i][0] == '.');
      if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
          (!is_ext && strstr(E.filename, s->filematch[i]))) {
        E.syntax = s;

        int filerow;
        for (filerow = 0; filerow < E.numrows; filerow++) {
          editorUpdateSyntax(&E.row[filerow]);
        }

        return;
      }
      i++;
    }
  }
}

/* row operations */

int editorRowCxToRx(erow *row, int cx) {
  int rx = 0;
  int j;
  for (j = 0; j < cx; j++) {
    if (row->chars[j] == '\t')
      rx += (FLUKE_TAB_STOP - 1) - (rx % FLUKE_TAB_STOP);
    rx++;
  }
  return rx;
}

int editorRowRxToCx(erow *row, int rx) {
  int cur_rx = 0;
  int cx;
  for (cx = 0; cx < row->size; cx++) {
    if (row->chars[cx] == '\t')
      cur_rx += (FLUKE_TAB_STOP - 1) - (cur_rx % FLUKE_TAB_STOP);
    cur_rx++;

    if (cur_rx > rx) return cx;
  }
  return cx;
}

void editorUpdateRow(erow *row) {
  int tabs = 0;
  int j;
  for (j = 0; j < row->size; j++)
    if (row->chars[j] == '\t') tabs++;

  free(row->render);
  row->render = malloc(row->size + tabs*(FLUKE_TAB_STOP - 1) + 1);

  int idx = 0;
  for (j = 0; j < row->size; j++) {
    if (row->chars[j] == '\t') {
      row->render[idx++] = ' ';
      while (idx % FLUKE_TAB_STOP != 0) row->render[idx++] = ' ';
    } else {
      row->render[idx++] = row->chars[j];
    }
  }
  row->render[idx] = '\0';
  row->rsize = idx;

  editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
  if (at < 0 || at > E.numrows) return;

  E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
  memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
  for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

  E.row[at].idx = at;

  E.row[at].size = len;
  E.row[at].chars = malloc(len + 1);
  memcpy(E.row[at].chars, s, len);
  E.row[at].chars[len] = '\0';

  E.row[at].rsize = 0;
  E.row[at].render = NULL;
  E.row[at].hl = NULL;
  E.row[at].hl_open_comment = 0;
  editorUpdateRow(&E.row[at]);

  E.numrows++;
  E.dirty++;
}

void editorFreeRow(erow *row) {
  free(row->render);
  free(row->chars);
  free(row->hl);
}

void editorDelRow(int at) {
  if (at < 0 || at >= E.numrows) return;
  editorFreeRow(&E.row[at]);
  memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
  for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
  E.numrows--;
  E.dirty++;
}

void editorRowInsertChar(erow *row, int at, int c) {
  if (at < 0 || at > row->size) at = row->size;
  row->chars = realloc(row->chars, row->size + 2);
  memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
  row->size++;
  row->chars[at] = c;
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
  row->chars = realloc(row->chars, row->size + len + 1);
  memcpy(&row->chars[row->size], s, len);
  row->size += len;
  row->chars[row->size] = '\0';
  editorUpdateRow(row);
  E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
  if (at < 0 || at >= row->size) return;
  memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
  row->size--;
  editorUpdateRow(row);
  E.dirty++;
}

/* editor operations */

void editorInsertChar(int c) {
  if (E.read_only) {
    editorSetStatusMessage("Cannot edit in read-only mode");
    return;
  }
  pushUndoSnapshot();
  if (E.cy == E.numrows) {
    editorInsertRow(E.numrows, "", 0);
  }
  editorRowInsertChar(&E.row[E.cy], E.cx, c);
  E.cx++;
}

void editorInsertNewline() {
  if (E.read_only) {
    editorSetStatusMessage("Cannot edit in read-only mode");
    return;
  }
  pushUndoSnapshot();
  
  /* calculate indentation from current line */
  int indent = 0;
  if (E.cy < E.numrows) {
    erow *row = &E.row[E.cy];
    while (indent < row->size && (row->chars[indent] == ' ' || row->chars[indent] == '\t')) {
      indent++;
    }
  }
  
  if (E.cx == 0) {
    editorInsertRow(E.cy, "", 0);
  } else {
    erow *row = &E.row[E.cy];
    editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
    row = &E.row[E.cy];
    row->size = E.cx;
    row->chars[row->size] = '\0';
    editorUpdateRow(row);
  }
  E.cy++;
  E.cx = 0;
  
  /* apply auto-indent */
  if (indent > 0 && E.cy < E.numrows) {
    erow *newrow = &E.row[E.cy];
    for (int i = 0; i < indent; i++) {
      char indent_char = (E.cy > 0 && i < E.row[E.cy - 1].size) ? E.row[E.cy - 1].chars[i] : ' ';
      if (indent_char != ' ' && indent_char != '\t') indent_char = ' ';
      editorRowInsertChar(newrow, E.cx, indent_char);
      E.cx++;
    }
  }
}

void editorDelChar() {
  if (E.cy == E.numrows) return;
  if (E.cx == 0 && E.cy == 0) return;
  if (E.read_only) {
    editorSetStatusMessage("Cannot edit in read-only mode");
    return;
  }

  pushUndoSnapshot();
  erow *row = &E.row[E.cy];
  if (E.cx > 0) {
    /* smart dedent: if we're at the beginning of indentation, remove a full indent level */
    int is_indent_only = 1;
    for (int i = 0; i < E.cx; i++) {
      if (row->chars[i] != ' ' && row->chars[i] != '\t') {
        is_indent_only = 0;
        break;
      }
    }
    
    if (is_indent_only && E.cx >= FLUKE_TAB_STOP) {
      /* remove up to tab stop worth of spaces */
      int to_remove = E.cx % FLUKE_TAB_STOP;
      if (to_remove == 0) to_remove = FLUKE_TAB_STOP;
      
      for (int i = 0; i < to_remove && E.cx > 0; i++) {
        editorRowDelChar(row, E.cx - 1);
        E.cx--;
      }
    } else {
      editorRowDelChar(row, E.cx - 1);
      E.cx--;
    }
  } else {
    E.cx = E.row[E.cy - 1].size;
    editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
    editorDelRow(E.cy);
    E.cy--;
  }
}

/* file i/o */

char *editorRowsToString(int *buflen) {
  int totlen = 0;
  int j;
  for (j = 0; j < E.numrows; j++)
    totlen += E.row[j].size + 1;
  *buflen = totlen;

  char *buf = malloc(totlen);
  char *p = buf;
  for (j = 0; j < E.numrows; j++) {
    memcpy(p, E.row[j].chars, E.row[j].size);
    p += E.row[j].size;
    *p = '\n';
    p++;
  }

  return buf;
}

void editorOpen(char *filename) {
  free(E.filename);
  E.filename = strdup(filename);

  editorSelectSyntaxHighlight();

  FILE *fp = fopen(filename, "r");
  if (!fp) die("fopen");

  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen;
  while ((linelen = getline(&line, &linecap, fp)) != -1) {
    while (linelen > 0 && (line[linelen - 1] == '\n' ||
                           line[linelen - 1] == '\r'))
      linelen--;
    editorInsertRow(E.numrows, line, linelen);
  }
  free(line);
  fclose(fp);
  E.dirty = 0;

  /* attempt to restore prior cursor position for this file */
  restoreCursorPositionIfAny();
  
  /* check for autosave recovery */
  editorCheckRecovery();
}

void editorSave() {
  if (E.filename == NULL) {
    E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
    if (E.filename == NULL) {
      editorSetStatusMessage("Save aborted");
      return;
    }
    editorSelectSyntaxHighlight();
  }

  int len;
  char *buf = editorRowsToString(&len);

  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirty = 0;
        editorSetStatusMessage("%d bytes written to disk", len);
        return;
      }
    }
    close(fd);
  }

  free(buf);
  editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

void editorSaveAs() {
  char *new_filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
  if (new_filename == NULL) {
    editorSetStatusMessage("Save as aborted");
    return;
  }

  char *old_filename = E.filename;
  E.filename = new_filename;
  editorSelectSyntaxHighlight();

  int len;
  char *buf = editorRowsToString(&len);

  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1) {
    if (ftruncate(fd, len) != -1) {
      if (write(fd, buf, len) == len) {
        close(fd);
        free(buf);
        E.dirty = 0;
        editorSetStatusMessage("%d bytes written to %s", len, E.filename);
        free(old_filename);
        return;
      }
    }
    close(fd);
  }

  /* restore old filename on error */
  free(E.filename);
  E.filename = old_filename;
  free(buf);
  editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/* find */

void editorFindCallback(char *query, int key) {
  static int last_match = -1;
  static int direction = 1;

  static int saved_hl_line;
  static char *saved_hl = NULL;

  /* Incremental: update global query for overlay highlighting */
  if (query && *query) {
    strncpy(g_search_query, query, sizeof(g_search_query) - 1);
    g_search_query[sizeof(g_search_query) - 1] = '\0';
  } else {
    g_search_query[0] = '\0';
  }

  if (saved_hl) {
    memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
    free(saved_hl);
    saved_hl = NULL;
  }

  if (key == '\r' || key == '\x1b') {
    last_match = -1;
    direction = 1;
    if (key == '\x1b') {
      /* cancel overlay highlight on escape */
      g_search_query[0] = '\0';
    }
    return;
  } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
    direction = 1;
  } else if (key == ARROW_LEFT || key == ARROW_UP) {
    direction = -1;
  } else {
    last_match = -1;
    direction = 1;
  }

  if (last_match == -1) direction = 1;
  int current = last_match;
  int i;
  for (i = 0; i < E.numrows; i++) {
    current += direction;
    if (current == -1) current = E.numrows - 1;
    else if (current == E.numrows) current = 0;

    erow *row = &E.row[current];
    char *match = strstr(row->render, query);
    if (match) {
      last_match = current;
      E.cy = current;
      E.cx = editorRowRxToCx(row, match - row->render);
      E.rowoff = E.numrows;

      saved_hl_line = current;
      saved_hl = malloc(row->rsize);
      memcpy(saved_hl, row->hl, row->rsize);
      memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
      break;
    }
  }
}

void editorFind() {
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_coloff = E.coloff;
  int saved_rowoff = E.rowoff;

  char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
                             editorFindCallback);

  if (query) {
    /* remember query for incremental highlight */
    if (*query) {
      strncpy(g_search_query, query, sizeof(g_search_query) - 1);
      g_search_query[sizeof(g_search_query) - 1] = '\0';
    } else {
      g_search_query[0] = '\0';
    }
    free(query);
  } else {
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.coloff = saved_coloff;
    E.rowoff = saved_rowoff;
  }
}

/* navigation utilities */

void editorGotoLine() {
  if (E.numrows == 0) {
    editorSetStatusMessage("Buffer is empty");
    return;
  }

  char *resp = editorPrompt("Go to line: %s", NULL);
  if (!resp) return;
  long val = strtol(resp, NULL, 10);
  free(resp);
  if (val <= 0) val = 1;
  if (val > E.numrows) val = E.numrows;
  E.cy = (int)val - 1;
  if (E.cy < 0) E.cy = 0;
  if (E.cy >= E.numrows) E.cy = E.numrows - 1;
  if (E.cy < 0) E.cy = 0;
  int rowlen = (E.cy >= 0 && E.cy < E.numrows) ? E.row[E.cy].size : 0;
  if (E.cx > rowlen) E.cx = rowlen;
  E.rowoff = E.cy;
  if (E.rowoff < 0) E.rowoff = 0;
  E.coloff = 0;
}

/* command palette and toggles */

static void editorToggleWrap() {
  E.soft_wrap_enabled = !E.soft_wrap_enabled;
  E.vrowoff = 0;
  editorSetStatusMessage("Wrap: %s", E.soft_wrap_enabled ? "ON" : "OFF");
}

static void editorToggleLineNumbers() {
  E.show_line_numbers = !E.show_line_numbers;
  editorSetStatusMessage("Line numbers: %s", E.show_line_numbers ? "ON" : "OFF");
}

static void editorQuitNow() {
  persistCursorPositionIfAny();
  editorCleanupAutosave();
  if (write(STDOUT_FILENO, "\x1b[2J", 4) == -1) { /* ignore error */ }
  if (write(STDOUT_FILENO, "\x1b[H", 3) == -1) { /* ignore error */ }
  exit(0);
}

static void editorCycleTheme() {
  current_theme = (current_theme + 1) % (sizeof(themes) / sizeof(themes[0]));
  editorSetStatusMessage("Theme: %s", themes[current_theme].name);
  
  /* refresh syntax highlighting for all rows */
  for (int i = 0; i < E.numrows; i++) {
    editorUpdateSyntax(&E.row[i]);
  }
}

static void editorSetMark(char mark) {
  if (mark < 'a' || mark > 'z') {
    editorSetStatusMessage("Invalid mark: %c (use a-z)", mark);
    return;
  }
  
  int idx = mark - 'a';
  E.marks[idx].cy = E.cy;
  E.marks[idx].cx = E.cx;
  E.marks[idx].set = 1;
  editorSetStatusMessage("Mark %c set at line %d, col %d", mark, E.cy + 1, E.cx + 1);
}

static void editorJumpToMark(char mark) {
  if (mark < 'a' || mark > 'z') {
    editorSetStatusMessage("Invalid mark: %c (use a-z)", mark);
    return;
  }
  
  int idx = mark - 'a';
  if (!E.marks[idx].set) {
    editorSetStatusMessage("Mark %c not set", mark);
    return;
  }
  
  E.cy = E.marks[idx].cy;
  E.cx = E.marks[idx].cx;
  
  /* validate position */
  if (E.cy >= E.numrows) E.cy = E.numrows ? E.numrows - 1 : 0;
  if (E.cy < 0) E.cy = 0;
  
  int rowlen = (E.cy < E.numrows) ? E.row[E.cy].size : 0;
  if (E.cx > rowlen) E.cx = rowlen;
  if (E.cx < 0) E.cx = 0;
  
  editorSetStatusMessage("Jumped to mark %c", mark);
}

static void editorToggleReadOnly() {
  E.read_only = !E.read_only;
  if (E.read_only && E.is_insert_mode) {
    E.is_insert_mode = 0; /* exit insert mode when entering read-only */
  }
  editorSetStatusMessage("Read-only: %s", E.read_only ? "ON" : "OFF");
}

static void editorShowHelp() {
  /* Clear screen and show help */
  if (write(STDOUT_FILENO, "\x1b[2J", 4) == -1) { /* ignore error */ }
  if (write(STDOUT_FILENO, "\x1b[H", 3) == -1) { /* ignore error */ }
  
  const char *help_text = 
    "\x1b[1;36m=== FLUKE EDITOR - KEYBINDING CHEATSHEET ===\x1b[0m\r\n\r\n"
    "\x1b[1;33mFILE OPERATIONS:\x1b[0m\r\n"
    "  Ctrl-S       Save file\r\n"
    "  Ctrl-E       Save as (new filename)\r\n"
    "  Ctrl-O       Open file (fuzzy finder)\r\n"
    "  Ctrl-Q       Quit (with confirmation if unsaved)\r\n\r\n"
    "\x1b[1;33mEDITING:\x1b[0m\r\n"
    "  i            Enter insert mode\r\n"
    "  Esc          Enter normal mode\r\n"
    "  x            Delete character (normal mode)\r\n"
    "  Ctrl-U       Undo\r\n"
    "  Ctrl-R       Redo\r\n\r\n"
    "\x1b[1;33mNAVIGATION:\x1b[0m\r\n"
    "  Arrow keys   Move cursor\r\n"
    "  h/j/k/l      Move cursor (normal mode)\r\n"
    "  w/b          Word motion forward/backward (normal mode)\r\n"
    "  Ctrl-Left    Move by word left (if supported)\r\n"
    "  Ctrl-Right   Move by word right (if supported)\r\n"
    "  Home/End     Beginning/end of line\r\n"
    "  Page Up/Down Page navigation\r\n"
    "  Ctrl-G       Go to line number\r\n\r\n"
    "\x1b[1;33mMARKS:\x1b[0m\r\n"
    "  m + letter   Set mark (ma, mb, etc.)\r\n"
    "  ' + letter   Jump to mark ('a, 'b, etc.)\r\n\r\n"
    "\x1b[1;33mSEARCH:\x1b[0m\r\n"
    "  Ctrl-F       Find/search with highlighting\r\n"
    "  Arrows       Navigate search results\r\n\r\n"
    "\x1b[1;33mVIEW OPTIONS:\x1b[0m\r\n"
    "  Ctrl-W       Toggle soft wrap\r\n"
    "  Ctrl-N       Toggle line numbers\r\n"
    "  Ctrl-T       Cycle themes\r\n"
    "  Ctrl-L       Toggle read-only mode\r\n\r\n"
    "\x1b[1;33mCOMMAND PALETTE:\x1b[0m\r\n"
    "  Ctrl-P       Open command palette\r\n"
    "  Commands:    save, saveas, quit, quit!, wrap, nowrap,\r\n"
    "               lines, nolines, goto, find, open, theme,\r\n"
    "               marks, readonly, config, help\r\n\r\n"
    "\x1b[1;33mCONFIGURATION:\x1b[0m\r\n"
    "  config       Save current settings to .flukerc\r\n"
    "  Auto-save    Automatic backup every 30 seconds\r\n"
    "  Recovery     Offers to restore from crash\r\n\r\n"
    "\x1b[1;33mOTHER:\x1b[0m\r\n"
    "  Ctrl-H       Show this help\r\n\r\n"
    "\x1b[1;32mPress any key to return to editor...\x1b[0m";
  
  if (write(STDOUT_FILENO, help_text, strlen(help_text)) == -1) { /* ignore error */ }
  
  /* Wait for any key */
  editorReadKey();
  
  /* Refresh the editor screen */
  editorRefreshScreen();
}

typedef struct CommandEntry {
  const char *name;
} CommandEntry;

/* Command palette data structure */
typedef struct {
  char **commands;
  int *scores;
  int count;
  int selected;
  char search[256];
} CommandPalette;

static CommandPalette g_palette = {0};

/* command list must be defined before use */
static const CommandEntry g_commands[] = {
  {"save"}, {"write"}, {"saveas"},
  {"quit"}, {"quit!"},
  {"wrap"}, {"nowrap"},
  {"lines"}, {"nolines"},
  {"goto"}, {"find"},
  {"open"}, {"theme"}, {"marks"}, {"readonly"},
  {"config"}, {"help"}
};

/* Command palette implementations */
static void freePalette() {
  if (g_palette.commands) {
    for (int i = 0; i < g_palette.count; i++) {
      free(g_palette.commands[i]);
    }
    free(g_palette.commands);
    free(g_palette.scores);
  }
  g_palette.commands = NULL;
  g_palette.scores = NULL;
  g_palette.count = 0;
  g_palette.selected = 0;
  g_palette.search[0] = '\0';
}

static void loadCommands() {
  freePalette();
  
  /* Load all available commands */
  const char *command_list[] = {
    "save", "write", "saveas",
    "quit", "quit!",
    "wrap", "nowrap",
    "lines", "nolines",
    "goto", "find",
    "open", "theme", "marks", "readonly",
    "config", "help"
  };
  
  int cmd_count = sizeof(command_list) / sizeof(command_list[0]);
  
  g_palette.commands = malloc(cmd_count * sizeof(char*));
  g_palette.scores = malloc(cmd_count * sizeof(int));
  g_palette.count = cmd_count;
  
  for (int i = 0; i < cmd_count; i++) {
    g_palette.commands[i] = strdup(command_list[i]);
    g_palette.scores[i] = 0;
  }
}

static void updatePaletteScores() {
  if (!g_palette.commands) return;
  
  for (int i = 0; i < g_palette.count; i++) {
    if (g_palette.search[0] == '\0') {
      g_palette.scores[i] = 1; /* show all commands when no search */
    } else {
      g_palette.scores[i] = fuzzyScore(g_palette.search, g_palette.commands[i]);
    }
  }
  
  /* Sort by score (simple bubble sort) */
  for (int i = 0; i < g_palette.count - 1; i++) {
    for (int j = 0; j < g_palette.count - i - 1; j++) {
      if (g_palette.scores[j] < g_palette.scores[j + 1]) {
        /* Swap scores */
        int tempScore = g_palette.scores[j];
        g_palette.scores[j] = g_palette.scores[j + 1];
        g_palette.scores[j + 1] = tempScore;
        
        /* Swap commands */
        char *tempCmd = g_palette.commands[j];
        g_palette.commands[j] = g_palette.commands[j + 1];
        g_palette.commands[j + 1] = tempCmd;
      }
    }
  }
  
  /* Reset selection to first visible command */
  g_palette.selected = 0;
  if (g_palette.search[0] != '\0') {
    for (int i = 0; i < g_palette.count; i++) {
      if (g_palette.scores[i] > 0) {
        g_palette.selected = i;
        break;
      }
    }
  }
}

static void drawCommandPalette(struct abuf *ab) {
  int maxRows = E.screenrows - 8;
  int maxCols = E.screencols - 6;
  int startRow = 3;
  int startCol = 3;
  
  /* Clear entire screen first */
  abAppend(ab, "\x1b[2J", 4);
  abAppend(ab, "\x1b[H", 3);
  
  /* Draw top border */
  char topBorder[256];
  snprintf(topBorder, sizeof(topBorder), "\x1b[%d;%dH", startRow, startCol);
  abAppend(ab, topBorder, strlen(topBorder));
  abAppend(ab, "\x1b[44;37m", 8); /* Blue background, white text */
  abAppend(ab, "+", 1);
  for (int i = 0; i < maxCols - 2; i++) abAppend(ab, "-", 1);
  abAppend(ab, "+", 1);
  abAppend(ab, "\x1b[m", 3); /* Reset */
  
  /* Draw title */
  char titleLine[256];
  snprintf(titleLine, sizeof(titleLine), "\x1b[%d;%dH\x1b[44;37m| COMMAND PALETTE - %d commands", 
           startRow + 1, startCol, g_palette.count);
  abAppend(ab, titleLine, strlen(titleLine));
  
  /* Pad title line */
  int titleLen = strlen("| COMMAND PALETTE - ") + snprintf(NULL, 0, "%d commands", g_palette.count);
  for (int i = titleLen; i < maxCols - 1; i++) abAppend(ab, " ", 1);
  abAppend(ab, "|\x1b[m", 5);
  
  /* Draw search line */
  char searchLine[512];
  int searchWidth = maxCols - 12;
  if (searchWidth < 0) searchWidth = 0;
  if (searchWidth > 200) searchWidth = 200;
  snprintf(searchLine, sizeof(searchLine), "\x1b[%d;%dH\x1b[44;37m| Search: %-*.*s|\x1b[m", 
           startRow + 2, startCol, searchWidth, searchWidth, g_palette.search);
  abAppend(ab, searchLine, strlen(searchLine));
  
  /* Draw separator */
  char sepLine[256];
  snprintf(sepLine, sizeof(sepLine), "\x1b[%d;%dH\x1b[44;37m+", startRow + 3, startCol);
  abAppend(ab, sepLine, strlen(sepLine));
  for (int i = 0; i < maxCols - 2; i++) abAppend(ab, "-", 1);
  abAppend(ab, "+\x1b[m", 5);
  
  /* Draw commands */
  int visibleCount = 0;
  for (int i = 0; i < g_palette.count && visibleCount < maxRows - 4; i++) {
    if (g_palette.scores[i] <= 0 && g_palette.search[0] != '\0') continue;
    
    char cmdLine[512];
    int row = startRow + 4 + visibleCount;
    
    if (i == g_palette.selected) {
      /* Selected command - highlighted */
      snprintf(cmdLine, sizeof(cmdLine), "\x1b[%d;%dH\x1b[47;30m|>%-*.*s|\x1b[m", 
               row, startCol, maxCols - 4, maxCols - 4, g_palette.commands[i]);
    } else {
      /* Normal command */
      snprintf(cmdLine, sizeof(cmdLine), "\x1b[%d;%dH\x1b[44;37m| %-*.*s|\x1b[m", 
               row, startCol, maxCols - 4, maxCols - 4, g_palette.commands[i]);
    }
    abAppend(ab, cmdLine, strlen(cmdLine));
    visibleCount++;
  }
  
  /* Fill remaining rows */
  for (int i = visibleCount; i < maxRows - 4; i++) {
    char emptyLine[256];
    snprintf(emptyLine, sizeof(emptyLine), "\x1b[%d;%dH\x1b[44;37m|", startRow + 4 + i, startCol);
    abAppend(ab, emptyLine, strlen(emptyLine));
    for (int j = 0; j < maxCols - 2; j++) abAppend(ab, " ", 1);
    abAppend(ab, "|\x1b[m", 5);
  }
  
  /* Draw bottom border */
  char bottomLine[256];
  snprintf(bottomLine, sizeof(bottomLine), "\x1b[%d;%dH\x1b[44;37m+", startRow + maxRows, startCol);
  abAppend(ab, bottomLine, strlen(bottomLine));
  for (int i = 0; i < maxCols - 2; i++) abAppend(ab, "-", 1);
  abAppend(ab, "+\x1b[m", 5);
  
  /* Draw help line */
  char helpLine[256];
  snprintf(helpLine, sizeof(helpLine), "\x1b[%d;%dH\x1b[43;30m ENTER=Execute | ESC=Cancel | UP/DOWN=Navigate | Type=Search \x1b[m", 
           startRow + maxRows + 1, startCol);
  abAppend(ab, helpLine, strlen(helpLine));
}

static void executeCommand(const char *cmd) {
  if (!strcmp(cmd, "save") || !strcmp(cmd, "write")) {
    editorSave();
  } else if (!strcmp(cmd, "saveas")) {
    editorSaveAs();
  } else if (!strcmp(cmd, "quit!")) {
    editorQuitNow();
  } else if (!strcmp(cmd, "quit")) {
    if (E.dirty) {
      editorSetStatusMessage("Unsaved changes. Use quit! or Ctrl-Q to force quit.");
    } else {
      editorQuitNow();
    }
  } else if (!strcmp(cmd, "wrap")) {
    if (!E.soft_wrap_enabled) editorToggleWrap(); else editorSetStatusMessage("Wrap: ON");
  } else if (!strcmp(cmd, "nowrap")) {
    if (E.soft_wrap_enabled) editorToggleWrap(); else editorSetStatusMessage("Wrap: OFF");
  } else if (!strcmp(cmd, "lines")) {
    if (!E.show_line_numbers) editorToggleLineNumbers(); else editorSetStatusMessage("Line numbers: ON");
  } else if (!strcmp(cmd, "nolines")) {
    if (E.show_line_numbers) editorToggleLineNumbers(); else editorSetStatusMessage("Line numbers: OFF");
  } else if (!strcmp(cmd, "goto")) {
    editorGotoLine();
  } else if (!strcmp(cmd, "find")) {
    editorFind();
  } else if (!strcmp(cmd, "open")) {
    editorOpenPrompt();
  } else if (!strcmp(cmd, "theme")) {
    editorCycleTheme();
  } else if (!strcmp(cmd, "marks")) {
    /* show all set marks */
    char msg[256] = "Set marks: ";
    int found = 0;
    for (int i = 0; i < 26; i++) {
      if (E.marks[i].set) {
        if (found > 0) strcat(msg, ", ");
        char mark_info[32];
        snprintf(mark_info, sizeof(mark_info), "%c(%d:%d)", 'a' + i, E.marks[i].cy + 1, E.marks[i].cx + 1);
        strcat(msg, mark_info);
        found++;
      }
    }
    if (found == 0) strcat(msg, "none");
    editorSetStatusMessage("%s", msg);
  } else if (!strcmp(cmd, "readonly")) {
    editorToggleReadOnly();
  } else if (!strcmp(cmd, "config")) {
    saveConfig();
    editorSetStatusMessage("Configuration saved to %s", ".flukerc");
  } else if (!strcmp(cmd, "help")) {
    editorShowHelp();
  } else {
    editorSetStatusMessage("Unknown command: %s", cmd);
  }
}

void editorCommandPalette() {
  loadCommands();
  updatePaletteScores();
  
  if (g_palette.count == 0) {
    editorSetStatusMessage("No commands available");
    freePalette();
    return;
  }
  
  /* Show cursor */
  write(STDOUT_FILENO, "\x1b[?25h", 6);
  
  while (1) {
    /* Draw palette */
    struct abuf ab = ABUF_INIT;
    drawCommandPalette(&ab);
    
    /* Position cursor in search box */
    char cursorPos[32];
    snprintf(cursorPos, sizeof(cursorPos), "\x1b[%d;%dH", 6, 13 + (int)strlen(g_palette.search));
    abAppend(&ab, cursorPos, strlen(cursorPos));
    
    /* Flush to screen */
    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
    
    /* Get user input */
    int c = editorReadKey();
    
    if (c == '\x1b') { /* Escape */
      editorSetStatusMessage("Command canceled");
      break;
    } else if (c == '\r') { /* Enter */
      if (g_palette.selected < g_palette.count) {
        char *selectedCmd = g_palette.commands[g_palette.selected];
        executeCommand(selectedCmd);
      }
      break;
    } else if (c == ARROW_UP) {
      if (g_palette.selected > 0) {
        g_palette.selected--;
        /* Skip invisible commands when searching */
        while (g_palette.selected > 0 && g_palette.search[0] != '\0' && g_palette.scores[g_palette.selected] <= 0) {
          g_palette.selected--;
        }
      }
    } else if (c == ARROW_DOWN) {
      if (g_palette.selected < g_palette.count - 1) {
        g_palette.selected++;
        /* Skip invisible commands when searching */
        while (g_palette.selected < g_palette.count - 1 && g_palette.search[0] != '\0' && g_palette.scores[g_palette.selected] <= 0) {
          g_palette.selected++;
        }
      }
    } else if (c == BACKSPACE || c == DEL_KEY || c == CTRL_KEY('h')) {
      /* Remove character from search */
      int len = strlen(g_palette.search);
      if (len > 0) {
        g_palette.search[len - 1] = '\0';
        updatePaletteScores();
      }
    } else if (!iscntrl(c) && c < 128) {
      /* Add character to search */
      size_t len = strlen(g_palette.search);
      if (len < sizeof(g_palette.search) - 1) {
        g_palette.search[len] = c;
        g_palette.search[len + 1] = '\0';
        updatePaletteScores();
      }
    }
  }
  
  freePalette();
  
  /* Hide cursor and restore screen */
  write(STDOUT_FILENO, "\x1b[?25l", 6);
  editorRefreshScreen();
}



/* File browser data structure */
typedef struct {
  char **files;
  int *scores;
  int count;
  int selected;
  char search[256];
} FileBrowser;

static FileBrowser g_browser = {0};

/* File browser implementation moved after abuf definition */
void editorOpenPrompt(); /* Forward declaration */

/* Old file prompt callback removed - using new file browser instead */

/* persistence of cursor position */
static const char *cursor_state_filename = ".fluke_cursor";

/* config system */
static const char *config_filename = ".flukerc";

/* autosave system */
static const char *autosave_prefix = ".fluke_autosave_";
static time_t last_autosave = 0;
static const int autosave_interval = 30; /* seconds */

typedef struct {
  int default_theme;
  int show_line_numbers;
  int soft_wrap_enabled;
  int tab_stop;
  int insert_mode_default;
} Config;

/* removed unused default_config */

static int getStateFilePath(char *out, size_t outsz) {
  const char *home = getenv("HOME");
  if (!home || !*home) home = ".";
  int n = snprintf(out, outsz, "%s/%s", home, cursor_state_filename);
  return (n > 0 && (size_t)n < outsz) ? 0 : -1;
}

static int makeAbsolutePath(const char *in, char *out, size_t outsz) {
  if (!in || !*in) return -1;
  char *res = realpath(in, out);
  if (res) return 0;
  /* if realpath fails (e.g., file not yet created), try to compose from CWD */
  char cwd[PATH_MAX];
  if (!getcwd(cwd, sizeof(cwd))) return -1;
  int n = snprintf(out, outsz, "%s/%s", cwd, in);
  return (n > 0 && (size_t)n < outsz) ? 0 : -1;
}

static void persistCursorPositionIfAny() {
  if (!E.filename) return;

  char state_path[PATH_MAX];
  if (getStateFilePath(state_path, sizeof(state_path)) != 0) return;

  char abspath[PATH_MAX];
  if (makeAbsolutePath(E.filename, abspath, sizeof(abspath)) != 0) return;

  /* read existing entries if any */
  FILE *fp = fopen(state_path, "r");
  char *buf = NULL; size_t cap = 0; ssize_t len;
  int updated = 0;
  size_t lines_cap = 0, lines_len = 0;
  char **lines = NULL;

  if (fp) {
    while ((len = getline(&buf, &cap, fp)) != -1) {
      if (len > 0 && (buf[len-1] == '\n' || buf[len-1] == '\r')) buf[--len] = '\0';
      /* parse path\trow\tcol */
      char *tab1 = strchr(buf, '\t');
      if (!tab1) {
        /* keep line as-is */
      } else {
        *tab1 = '\0';
        const char *path = buf;
        char *tab2 = strchr(tab1 + 1, '\t');
        if (tab2) *tab2 = '\0';
        if (strcmp(path, abspath) == 0) {
          /* replace with new entry */
          char newline[256];
          int n = snprintf(newline, sizeof(newline), "%s\t%d\t%d", abspath, E.cy, E.cx);
          if (n > 0) {
            if (lines_len == lines_cap) {
              lines_cap = lines_cap ? lines_cap * 2 : 8;
              lines = realloc(lines, lines_cap * sizeof(char*));
            }
            lines[lines_len++] = strndup(newline, (size_t)n);
            updated = 1;
            continue; /* skip adding original */
          }
        }
        if (tab2) *tab2 = '\t';
        *tab1 = '\t';
      }
      if (lines_len == lines_cap) {
        lines_cap = lines_cap ? lines_cap * 2 : 8;
        lines = realloc(lines, lines_cap * sizeof(char*));
      }
      lines[lines_len++] = strndup(buf, (size_t)len);
    }
    fclose(fp);
    free(buf);
  }

  if (!updated) {
    char newline[256];
    int n = snprintf(newline, sizeof(newline), "%s\t%d\t%d", abspath, E.cy, E.cx);
    if (n > 0) {
      if (lines_len == lines_cap) {
        lines_cap = lines_cap ? lines_cap * 2 : 8;
        lines = realloc(lines, lines_cap * sizeof(char*));
      }
      lines[lines_len++] = strndup(newline, (size_t)n);
    }
  }

  /* write back */
  fp = fopen(state_path, "w");
  if (!fp) {
    for (size_t i = 0; i < lines_len; i++) free(lines[i]);
    free(lines);
    return;
  }
  for (size_t i = 0; i < lines_len; i++) {
    fputs(lines[i], fp);
    fputc('\n', fp);
    free(lines[i]);
  }
  free(lines);
  fclose(fp);
}

static void restoreCursorPositionIfAny() {
  if (!E.filename) return;
  char state_path[PATH_MAX];
  if (getStateFilePath(state_path, sizeof(state_path)) != 0) return;
  char abspath[PATH_MAX];
  if (makeAbsolutePath(E.filename, abspath, sizeof(abspath)) != 0) return;

  FILE *fp = fopen(state_path, "r");
  if (!fp) return;

  char *line = NULL; size_t cap = 0; ssize_t len;
  while ((len = getline(&line, &cap, fp)) != -1) {
    if (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) line[--len] = '\0';
    char *tab1 = strchr(line, '\t');
    if (!tab1) continue;
    *tab1 = '\0';
    if (strcmp(line, abspath) != 0) { *tab1 = '\t'; continue; }
    char *tab2 = strchr(tab1 + 1, '\t');
    if (!tab2) { *tab1 = '\t'; continue; }
    *tab2 = '\0';
    int row = atoi(tab1 + 1);
    int col = atoi(tab2 + 1);
    if (row < 0) row = 0;
    if (row >= E.numrows) row = E.numrows ? E.numrows - 1 : 0;
    E.cy = row;
    int rowlen = (E.cy >= 0 && E.cy < E.numrows) ? E.row[E.cy].size : 0;
    if (col < 0) col = 0;
    if (col > rowlen) col = rowlen;
    E.cx = col;
    break;
  }
  free(line);
  fclose(fp);
}

/* very small fuzzy scorer: subsequence match weighted by adjacency */
static int fuzzyScore(const char *pattern, const char *candidate) {
  if (!pattern || !*pattern) return 0;
  if (!candidate || !*candidate) return 0;
  int score = 0;
  int consec = 0;
  const char *p = pattern;
  for (const char *c = candidate; *c && *p; c++) {
    if (tolower((unsigned char)*c) == tolower((unsigned char)*p)) {
      score += 1 + consec; /* reward consecutive */
      consec++;
      p++;
    } else {
      consec = 0;
    }
  }
  if (*p) return 0; /* not all pattern chars matched in order */
  return score;
}

/* config loading */
static void loadConfig() {
  FILE *fp = fopen(config_filename, "r");
  if (!fp) return; /* use defaults */
  
  char line[256];
  while (fgets(line, sizeof(line), fp)) {
    /* remove newline */
    char *newline = strchr(line, '\n');
    if (newline) *newline = '\0';
    
    /* skip comments and empty lines */
    if (line[0] == '#' || line[0] == '\0') continue;
    
    /* parse key=value */
    char *eq = strchr(line, '=');
    if (!eq) continue;
    *eq = '\0';
    char *key = line;
    char *value = eq + 1;
    
    /* trim whitespace */
    while (*key && isspace((unsigned char)*key)) key++;
    while (*value && isspace((unsigned char)*value)) value++;
    
    /* apply settings */
    if (strcmp(key, "theme") == 0) {
      int theme_num = atoi(value);
      if (theme_num >= 0 && theme_num < (int)(sizeof(themes) / sizeof(themes[0]))) {
        current_theme = theme_num;
      }
    } else if (strcmp(key, "line_numbers") == 0) {
      /* Trim trailing whitespace from value */
      char *end = value + strlen(value) - 1;
      while (end > value && isspace((unsigned char)*end)) end--;
      end[1] = '\0';
      
      E.show_line_numbers = (strcmp(value, "true") == 0 || strcmp(value, "1") == 0);
    } else if (strcmp(key, "soft_wrap") == 0) {
      E.soft_wrap_enabled = (strcmp(value, "true") == 0 || strcmp(value, "1") == 0);
    } else if (strcmp(key, "insert_mode") == 0) {
      E.is_insert_mode = (strcmp(value, "true") == 0 || strcmp(value, "1") == 0);
    }
  }
  
  fclose(fp);
}

static void saveConfig() {
  FILE *fp = fopen(config_filename, "w");
  if (!fp) return;
  
  fprintf(fp, "# Fluke Editor Configuration\n");
  fprintf(fp, "# Theme: 0=default, 1=dark, 2=light, 3=monokai, 4=solarized\n");
  fprintf(fp, "theme=%d\n", current_theme);
  fprintf(fp, "line_numbers=%s\n", E.show_line_numbers ? "true" : "false");
  fprintf(fp, "soft_wrap=%s\n", E.soft_wrap_enabled ? "true" : "false");
  fprintf(fp, "insert_mode=%s\n", E.is_insert_mode ? "true" : "false");
  
  fclose(fp);
}

/* autosave functionality */
static void getAutosaveFilename(char *buf, size_t bufsz) {
  if (E.filename) {
    snprintf(buf, bufsz, "%s%s", autosave_prefix, E.filename);
  } else {
    snprintf(buf, bufsz, "%suntitled", autosave_prefix);
  }
}

static void editorAutosave() {
  if (!E.dirty) return; /* no changes to save */
  
  time_t now = time(NULL);
  if (now - last_autosave < autosave_interval) return; /* too soon */
  
  char autosave_file[256];
  getAutosaveFilename(autosave_file, sizeof(autosave_file));
  
  int len;
  char *buf = editorRowsToString(&len);
  
  FILE *fp = fopen(autosave_file, "w");
  if (fp) {
    fwrite(buf, 1, len, fp);
    fclose(fp);
    last_autosave = now;
  }
  
  free(buf);
}

static void editorCheckRecovery() {
  if (!E.filename) return;
  
  char autosave_file[256];
  getAutosaveFilename(autosave_file, sizeof(autosave_file));
  
  struct stat autosave_stat, original_stat;
  if (stat(autosave_file, &autosave_stat) != 0) return; /* no autosave file */
  
  if (stat(E.filename, &original_stat) == 0) {
    /* both files exist, check if autosave is newer */
    if (autosave_stat.st_mtime <= original_stat.st_mtime) {
      unlink(autosave_file); /* autosave is older, remove it */
      return;
    }
  }
  
  /* offer recovery */
  char *response = editorPrompt("Autosave found. Recover? (y/N): %s", NULL);
  if (response && (response[0] == 'y' || response[0] == 'Y')) {
    /* load autosave file */
    FILE *fp = fopen(autosave_file, "r");
    if (fp) {
      /* clear current buffer */
      for (int i = 0; i < E.numrows; i++) editorFreeRow(&E.row[i]);
      free(E.row);
      E.row = NULL;
      E.numrows = 0;
      
      /* load from autosave */
      char *line = NULL;
      size_t linecap = 0;
      ssize_t linelen;
      while ((linelen = getline(&line, &linecap, fp)) != -1) {
        while (linelen > 0 && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r'))
          linelen--;
        editorInsertRow(E.numrows, line, linelen);
      }
      free(line);
      fclose(fp);
      
      E.dirty = 1; /* mark as modified */
      editorSetStatusMessage("Recovered from autosave");
    }
  }
  
  if (response) free(response);
  unlink(autosave_file); /* remove autosave file after recovery attempt */
}

static void editorCleanupAutosave() {
  if (!E.filename) return;
  
  char autosave_file[256];
  getAutosaveFilename(autosave_file, sizeof(autosave_file));
  unlink(autosave_file); /* remove autosave file on clean exit */
}

/* clear current buffer contents */
static void editorClearBuffer() {
  for (int i = 0; i < E.numrows; i++) editorFreeRow(&E.row[i]);
  free(E.row);
  E.row = NULL;
  E.numrows = 0;
  E.cx = 0; E.cy = 0; E.rx = 0;
  E.rowoff = 0; E.coloff = 0; E.vrowoff = 0;
  E.dirty = 0;
}

/* append buffer functions moved above */

/* File browser implementations */
static void freeBrowser() {
  if (g_browser.files) {
    for (int i = 0; i < g_browser.count; i++) {
      free(g_browser.files[i]);
    }
    free(g_browser.files);
    free(g_browser.scores);
  }
  g_browser.files = NULL;
  g_browser.scores = NULL;
  g_browser.count = 0;
  g_browser.selected = 0;
  g_browser.search[0] = '\0';
}

static void loadFiles() {
  freeBrowser();
  
  DIR *dir = opendir(".");
  if (!dir) return;
  
  /* Count files first */
  struct dirent *ent;
  int fileCount = 0;
  while ((ent = readdir(dir))) {
    if (ent->d_name[0] == '.') continue;
    fileCount++;
  }
  rewinddir(dir);
  
  if (fileCount == 0) {
    closedir(dir);
    return;
  }
  
  /* Allocate arrays */
  g_browser.files = malloc(fileCount * sizeof(char*));
  g_browser.scores = malloc(fileCount * sizeof(int));
  g_browser.count = 0;
  
  /* Load files */
  while ((ent = readdir(dir))) {
    if (ent->d_name[0] == '.') continue;
    g_browser.files[g_browser.count] = strdup(ent->d_name);
    g_browser.scores[g_browser.count] = 0;
    g_browser.count++;
  }
  closedir(dir);
}

static void updateBrowserScores() {
  if (!g_browser.files) return;
  
  for (int i = 0; i < g_browser.count; i++) {
    if (g_browser.search[0] == '\0') {
      g_browser.scores[i] = 1; /* show all files when no search */
    } else {
      g_browser.scores[i] = fuzzyScore(g_browser.search, g_browser.files[i]);
    }
  }
  
  /* Sort by score (simple bubble sort) */
  for (int i = 0; i < g_browser.count - 1; i++) {
    for (int j = 0; j < g_browser.count - i - 1; j++) {
      if (g_browser.scores[j] < g_browser.scores[j + 1]) {
        /* Swap scores */
        int tempScore = g_browser.scores[j];
        g_browser.scores[j] = g_browser.scores[j + 1];
        g_browser.scores[j + 1] = tempScore;
        
        /* Swap files */
        char *tempFile = g_browser.files[j];
        g_browser.files[j] = g_browser.files[j + 1];
        g_browser.files[j + 1] = tempFile;
      }
    }
  }
  
  /* Reset selection to first visible file */
  g_browser.selected = 0;
  if (g_browser.search[0] != '\0') {
    for (int i = 0; i < g_browser.count; i++) {
      if (g_browser.scores[i] > 0) {
        g_browser.selected = i;
        break;
      }
    }
  }
}

static void drawFileBrowser(struct abuf *ab) {
  int maxRows = E.screenrows - 8; /* Leave more space */
  int maxCols = E.screencols - 6;
  int startRow = 3;
  int startCol = 3;
  
  /* Clear entire screen first */
  abAppend(ab, "\x1b[2J", 4);
  abAppend(ab, "\x1b[H", 3);
  
  /* Draw top border */
  char topBorder[256];
  snprintf(topBorder, sizeof(topBorder), "\x1b[%d;%dH", startRow, startCol);
  abAppend(ab, topBorder, strlen(topBorder));
  abAppend(ab, "\x1b[44;37m", 8); /* Blue background, white text */
  abAppend(ab, "+", 1);
  for (int i = 0; i < maxCols - 2; i++) abAppend(ab, "-", 1);
  abAppend(ab, "+", 1);
  abAppend(ab, "\x1b[m", 3); /* Reset */
  
  /* Draw title */
  char titleLine[256];
  snprintf(titleLine, sizeof(titleLine), "\x1b[%d;%dH\x1b[44;37m| FILE BROWSER - %d files found", 
           startRow + 1, startCol, g_browser.count);
  abAppend(ab, titleLine, strlen(titleLine));
  
  /* Pad title line */
  int titleLen = strlen("| FILE BROWSER - ") + snprintf(NULL, 0, "%d files found", g_browser.count);
  for (int i = titleLen; i < maxCols - 1; i++) abAppend(ab, " ", 1);
  abAppend(ab, "|\x1b[m", 5);
  
  /* Draw search line */
  char searchLine[512];
  int searchWidth = maxCols - 12;
  if (searchWidth < 0) searchWidth = 0;
  if (searchWidth > 200) searchWidth = 200; /* Reasonable limit */
  snprintf(searchLine, sizeof(searchLine), "\x1b[%d;%dH\x1b[44;37m| Search: %-*.*s|\x1b[m", 
           startRow + 2, startCol, searchWidth, searchWidth, g_browser.search);
  abAppend(ab, searchLine, strlen(searchLine));
  
  /* Draw separator */
  char sepLine[256];
  snprintf(sepLine, sizeof(sepLine), "\x1b[%d;%dH\x1b[44;37m+", startRow + 3, startCol);
  abAppend(ab, sepLine, strlen(sepLine));
  for (int i = 0; i < maxCols - 2; i++) abAppend(ab, "-", 1);
  abAppend(ab, "+\x1b[m", 5);
  
  /* Draw files */
  int visibleCount = 0;
  for (int i = 0; i < g_browser.count && visibleCount < maxRows - 4; i++) {
    if (g_browser.scores[i] <= 0 && g_browser.search[0] != '\0') continue;
    
    char fileLine[512];
    int row = startRow + 4 + visibleCount;
    
    if (i == g_browser.selected) {
      /* Selected file - highlighted */
      snprintf(fileLine, sizeof(fileLine), "\x1b[%d;%dH\x1b[47;30m|>%-*.*s|\x1b[m", 
               row, startCol, maxCols - 4, maxCols - 4, g_browser.files[i]);
    } else {
      /* Normal file */
      snprintf(fileLine, sizeof(fileLine), "\x1b[%d;%dH\x1b[44;37m| %-*.*s|\x1b[m", 
               row, startCol, maxCols - 4, maxCols - 4, g_browser.files[i]);
    }
    abAppend(ab, fileLine, strlen(fileLine));
    visibleCount++;
  }
  
  /* Fill remaining rows */
  for (int i = visibleCount; i < maxRows - 4; i++) {
    char emptyLine[256];
    snprintf(emptyLine, sizeof(emptyLine), "\x1b[%d;%dH\x1b[44;37m|", startRow + 4 + i, startCol);
    abAppend(ab, emptyLine, strlen(emptyLine));
    for (int j = 0; j < maxCols - 2; j++) abAppend(ab, " ", 1);
    abAppend(ab, "|\x1b[m", 5);
  }
  
  /* Draw bottom border */
  char bottomLine[256];
  snprintf(bottomLine, sizeof(bottomLine), "\x1b[%d;%dH\x1b[44;37m+", startRow + maxRows, startCol);
  abAppend(ab, bottomLine, strlen(bottomLine));
  for (int i = 0; i < maxCols - 2; i++) abAppend(ab, "-", 1);
  abAppend(ab, "+\x1b[m", 5);
  
  /* Draw help line */
  char helpLine[256];
  snprintf(helpLine, sizeof(helpLine), "\x1b[%d;%dH\x1b[43;30m ENTER=Open | ESC=Cancel | UP/DOWN=Navigate | Type=Search \x1b[m", 
           startRow + maxRows + 1, startCol);
  abAppend(ab, helpLine, strlen(helpLine));
}

/* File browser main function - real implementation */
void editorOpenPrompt() {
  if (E.dirty) {
    char *ans = editorPrompt("Discard changes? y/N: %s", NULL);
    if (!ans) return; /* ESC cancels */
    int discard = (ans[0] == 'y' || ans[0] == 'Y');
    free(ans);
    if (!discard) { editorSetStatusMessage("Open canceled"); return; }
  }
  
  loadFiles();
  updateBrowserScores();
  
  if (g_browser.count == 0) {
    editorSetStatusMessage("No files found in current directory");
    freeBrowser();
    return;
  }
  
  /* Show cursor */
  if (write(STDOUT_FILENO, "\x1b[?25h", 6) == -1) { /* ignore error */ }
  
  while (1) {
    /* Draw browser */
    struct abuf ab = ABUF_INIT;
    drawFileBrowser(&ab);
    
    /* Position cursor in search box */
    char cursorPos[32];
    snprintf(cursorPos, sizeof(cursorPos), "\x1b[%d;%dH", 6, 13 + (int)strlen(g_browser.search));
    abAppend(&ab, cursorPos, strlen(cursorPos));
    
    /* Flush to screen */
    if (write(STDOUT_FILENO, ab.b, ab.len) == -1) { /* ignore error */ }
    abFree(&ab);
    
    /* Get user input */
    int c = editorReadKey();
    
    if (c == '\x1b') { /* Escape */
      editorSetStatusMessage("Open canceled");
      break;
    } else if (c == '\r') { /* Enter */
      if (g_browser.selected < g_browser.count) {
        char *selectedFile = g_browser.files[g_browser.selected];
        editorClearBuffer();
        editorOpen(selectedFile);
        editorSetStatusMessage("Opened: %s", selectedFile);
      }
      break;
    } else if (c == ARROW_UP) {
      if (g_browser.selected > 0) {
        g_browser.selected--;
        /* Skip invisible files when searching */
        while (g_browser.selected > 0 && g_browser.search[0] != '\0' && g_browser.scores[g_browser.selected] <= 0) {
          g_browser.selected--;
        }
      }
    } else if (c == ARROW_DOWN) {
      if (g_browser.selected < g_browser.count - 1) {
        g_browser.selected++;
        /* Skip invisible files when searching */
        while (g_browser.selected < g_browser.count - 1 && g_browser.search[0] != '\0' && g_browser.scores[g_browser.selected] <= 0) {
          g_browser.selected++;
        }
      }
    } else if (c == BACKSPACE || c == DEL_KEY || c == CTRL_KEY('h')) {
      /* Remove character from search */
      int len = strlen(g_browser.search);
      if (len > 0) {
        g_browser.search[len - 1] = '\0';
        updateBrowserScores();
      }
    } else if (!iscntrl(c) && c < 128) {
      /* Add character to search */
      size_t len = strlen(g_browser.search);
      if (len < sizeof(g_browser.search) - 1) {
        g_browser.search[len] = c;
        g_browser.search[len + 1] = '\0';
        updateBrowserScores();
      }
    }
  }
  
  freeBrowser();
  
  /* Hide cursor and restore screen */
  write(STDOUT_FILENO, "\x1b[?25l", 6);
  editorRefreshScreen();
}



/* output */

void editorScroll() {
  E.rx = 0;
  if (E.cy < E.numrows) {
    E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
  }

  if (!E.soft_wrap_enabled) {
    if (E.cy < E.rowoff) {
      E.rowoff = E.cy;
    }
    if (E.cy >= E.rowoff + E.screenrows) {
      E.rowoff = E.cy - E.screenrows + 1;
    }
    if (E.rx < E.coloff) {
      E.coloff = E.rx;
    }
    if (E.rx >= E.coloff + E.screencols) {
      E.coloff = E.rx - E.screencols + 1;
    }
  } else {
    /* visual-row based vertical scroll; horizontal scroll disabled when wrapping */
    E.coloff = 0;

    /* compute visual row index of (E.cy, E.rx) and adjust E.vrowoff */
    int vrow = 0;
    for (int r = 0; r < E.cy; r++) {
      int len = E.row[r].rsize;
      int wraps = len > 0 ? (len - 1) / (E.screencols - FLUKE_LINE_NUMBER_WIDTH) : 0;
      vrow += 1 + wraps;
    }
    /* curline_wraps calculation removed as it was unused */
    int line_vrow = vrow;

    if (line_vrow < E.vrowoff) {
      E.vrowoff = line_vrow;
    }
    if (line_vrow > E.vrowoff + E.screenrows - 1) {
      E.vrowoff = line_vrow - (E.screenrows - 1);
    }
  }
}

void editorDrawRows(struct abuf *ab) {
  int y;
  for (y = 0; y < E.screenrows; y++) {
    int filerow = E.soft_wrap_enabled ? 0 : (y + E.rowoff);
    int start_col = 0;
    int visual_index = y + (E.soft_wrap_enabled ? E.vrowoff : 0);

    if (E.soft_wrap_enabled) {
      /* map visual_index to (filerow,start_col) */
      int acc = 0;
      for (filerow = 0; filerow < E.numrows; filerow++) {
        int len = E.row[filerow].rsize;
        int cols = E.screencols - FLUKE_LINE_NUMBER_WIDTH;
        int wraps = (len <= 0) ? 0 : (len - 1) / cols;
        int segs = 1 + wraps;
        if (visual_index < acc + segs) {
          int within = visual_index - acc;
          start_col = within * cols;
          break;
        }
        acc += segs;
      }
    }

    if (filerow >= E.numrows) {
      if (E.numrows == 0 && y == E.screenrows / 3) {
        char welcome[80];
        int welcomelen = snprintf(welcome, sizeof(welcome),
          "Fluke editor -- version %s", FLUKE_VERSION);
        if (welcomelen > E.screencols) welcomelen = E.screencols;
        int padding = (E.screencols - welcomelen) / 2;
        if (padding) {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--) abAppend(ab, " ", 1);
        abAppend(ab, welcome, welcomelen);
      } else {
        abAppend(ab, "~", 1);
      }
    } else {
      char linenum[16];
      if (E.show_line_numbers) {
        snprintf(linenum, sizeof(linenum), "%*d ", FLUKE_LINE_NUMBER_WIDTH - 1, filerow + 1);
        char color_buf[16];
        snprintf(color_buf, sizeof(color_buf), "\x1b[%dm", themes[current_theme].line_number);
        abAppend(ab, color_buf, strlen(color_buf));
        abAppend(ab, linenum, strlen(linenum));
        abAppend(ab, "\x1b[39m", 5); // Reset to default color
      } else {
        /* keep gutter spacing for consistent layout */
        for (int i = 0; i < FLUKE_LINE_NUMBER_WIDTH; i++) {
          abAppend(ab, " ", 1);
        }
      }
      int effective_coloff = E.soft_wrap_enabled ? start_col : E.coloff;
      int len = E.row[filerow].rsize - effective_coloff;
      if (len < 0) len = 0;
      if (len > E.screencols - FLUKE_LINE_NUMBER_WIDTH) len = E.screencols - FLUKE_LINE_NUMBER_WIDTH;
      char *c = &E.row[filerow].render[effective_coloff];
      unsigned char *hl = &E.row[filerow].hl[effective_coloff];
      int current_color = -1;
      int j;
      int qlen = (int)strlen(g_search_query);
      for (j = 0; j < len; j++) {
        if (qlen > 0 && !iscntrl(c[j]) && j + qlen <= len &&
            memcmp(&c[j], g_search_query, (size_t)qlen) == 0) {
          int match_color = editorSyntaxToColor(HL_MATCH);
          if (match_color != current_color) {
            current_color = match_color;
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", match_color);
            abAppend(ab, buf, clen);
          }
          abAppend(ab, &c[j], qlen);
          j += qlen - 1;
          continue;
        }
        if (iscntrl(c[j])) {
          char sym = (c[j] <= 26) ? '@' + c[j] : '?';
          abAppend(ab, "\x1b[7m", 4);
          abAppend(ab, &sym, 1);
          abAppend(ab, "\x1b[m", 3);
          if (current_color != -1) {
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
            abAppend(ab, buf, clen);
          }
        } else if (hl[j] == HL_NORMAL) {
          if (current_color != -1) {
            abAppend(ab, "\x1b[39m", 5);
            current_color = -1;
          }
          abAppend(ab, &c[j], 1);
        } else {
          int color = editorSyntaxToColor(hl[j]);
          if (color != current_color) {
            current_color = color;
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
            abAppend(ab, buf, clen);
          }
          abAppend(ab, &c[j], 1);
        }
      }
      abAppend(ab, "\x1b[39m", 5);
    }

    abAppend(ab, "\x1b[K", 3);
    abAppend(ab, "\r\n", 2);
  }
}

void editorDrawStatusBar(struct abuf *ab) {
  char status_color[16];
  snprintf(status_color, sizeof(status_color), "\x1b[%d;%dm", 
           themes[current_theme].status_bg, themes[current_theme].status_fg);
  abAppend(ab, status_color, strlen(status_color));
  char status[80], rstatus[80];
  const char *mode = E.read_only ? "[RO]" : (E.is_insert_mode ? "[INS]" : "[NOR]");
  int len = snprintf(status, sizeof(status), "%.20s %s - %d lines %s",
    E.filename ? E.filename : "[No Name]", mode, E.numrows,
    E.dirty ? "(modified)" : "");
  int col = E.rx + 1;
  int row = E.cy + 1;
  int percent = (E.numrows ? (row * 100 / E.numrows) : 0);
  int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d:%d (%d%%) %s",
    E.syntax ? E.syntax->filetype : "no ft", row, col, percent,
    E.soft_wrap_enabled ? "WRAP" : "NOWRAP");
  if (len > E.screencols) len = E.screencols;
  abAppend(ab, status, len);
  while (len < E.screencols) {
    if (E.screencols - len == rlen) {
      abAppend(ab, rstatus, rlen);
      break;
    } else {
      abAppend(ab, " ", 1);
      len++;
    }
  }
  abAppend(ab, "\x1b[m", 3);
  abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
  abAppend(ab, "\x1b[K", 3);
  int msglen = strlen(E.statusmsg);
  if (msglen > E.screencols) msglen = E.screencols;
  if (msglen && time(NULL) - E.statusmsg_time < 5)
    abAppend(ab, E.statusmsg, msglen);
}

void editorRefreshScreen() {
  editorScroll();

  struct abuf ab = ABUF_INIT;

  abAppend(&ab, "\x1b[?25l", 6);
  abAppend(&ab, "\x1b[H", 3);

  editorDrawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
                                            (E.rx - (E.soft_wrap_enabled ? 0 : E.coloff)) + FLUKE_LINE_NUMBER_WIDTH + 1);
  abAppend(&ab, buf, strlen(buf));

  abAppend(&ab, "\x1b[?25h", 6);

  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

/* input */

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
  size_t bufsize = 128;
  char *buf = malloc(bufsize);

  size_t buflen = 0;
  buf[0] = '\0';

  while (1) {
    editorSetStatusMessage(prompt, buf);
    editorRefreshScreen();

    int c = editorReadKey();
    if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
      if (buflen != 0) buf[--buflen] = '\0';
    } else if (c == '\x1b') {
      editorSetStatusMessage("");
      if (callback) callback(buf, c);
      free(buf);
      return NULL;
    } else if (c == '\r') {
      if (buflen != 0) {
        editorSetStatusMessage("");
        if (callback) callback(buf, c);
        return buf;
      }
    } else if (!iscntrl(c) && c < 128) {
      if (buflen == bufsize - 1) {
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      buf[buflen++] = c;
      buf[buflen] = '\0';
    }

    if (callback) callback(buf, c);
  }
}

void editorMoveCursor(int key) {
  erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];

  switch (key) {
    case ARROW_LEFT:
      if (E.cx != 0) {
        E.cx--;
      } else if (E.cy > 0) {
        E.cy--;
        E.cx = E.row[E.cy].size;
      }
      break;
    case ARROW_RIGHT:
      if (row && E.cx < row->size) {
        E.cx++;
      } else if (row && E.cx == row->size) {
        E.cy++;
        E.cx = 0;
      }
      break;
    case ARROW_UP:
      if (E.cy != 0) {
        E.cy--;
      }
      break;
    case ARROW_DOWN:
      if (E.cy < E.numrows) {
        E.cy++;
      }
      break;
    case CTRL_ARROW_LEFT:
      /* Move to beginning of current word or previous word */
      if (row) {
        while (E.cx > 0 && !isalnum(row->chars[E.cx - 1]) && row->chars[E.cx - 1] != '_') E.cx--;
        while (E.cx > 0 && (isalnum(row->chars[E.cx - 1]) || row->chars[E.cx - 1] == '_')) E.cx--;
      }
      break;
    case CTRL_ARROW_RIGHT:
      /* Move to end of current word or next word */
      if (row) {
        while (E.cx < row->size && !isalnum(row->chars[E.cx]) && row->chars[E.cx] != '_') E.cx++;
        while (E.cx < row->size && (isalnum(row->chars[E.cx]) || row->chars[E.cx] == '_')) E.cx++;
      }
      break;
  }

  row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
  int rowlen = row ? row->size : 0;
  if (E.cx > rowlen) {
    E.cx = rowlen;
  }
}

void editorProcessKeypress() {
  static int quit_times = FLUKE_QUIT_TIMES;

  int c = editorReadKey();

  switch (c) {
    case '\r':
      if (E.is_insert_mode) editorInsertNewline();
      break;

    case CTRL_KEY('q'):
      if (E.dirty && quit_times > 0) {
        editorSetStatusMessage("WARNING!!! File has unsaved changes. "
          "Press Ctrl-Q %d more times to quit.", quit_times);
        quit_times--;
        return;
      }
      write(STDOUT_FILENO, "\x1b[2J", 4);
      write(STDOUT_FILENO, "\x1b[H", 3);
      exit(0);
      break;

    case CTRL_KEY('s'):
      editorSave();
      break;
      
    case CTRL_KEY('e'):
      editorSaveAs();
      break;



    case HOME_KEY:
      E.cx = 0;
      break;

    case END_KEY:
      if (E.cy < E.numrows)
        E.cx = E.row[E.cy].size;
      break;

    case CTRL_KEY('f'):
      editorFind();
      break;

    case CTRL_KEY('g'):
      editorGotoLine();
      break;

    case CTRL_KEY('w'):
      editorToggleWrap();
      break;

    case CTRL_KEY('n'):
      editorToggleLineNumbers();
      break;

    case CTRL_KEY('p'):
      editorCommandPalette();
      break;

    case CTRL_KEY('o'):
      editorOpenPrompt();
      break;

    case CTRL_KEY('u'):
      undoAction();
      break;

    case CTRL_KEY('r'):
      redoAction();
      break;

    case CTRL_KEY('t'):
      editorCycleTheme();
      break;

    case CTRL_KEY('l'):
      editorToggleReadOnly();
      break;

    case CTRL_KEY('h'):
      editorShowHelp();
      break;

    case BACKSPACE:
    case DEL_KEY:
      if (!E.is_insert_mode) break;
      if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
      editorDelChar();
      break;

    case PAGE_UP:
    case PAGE_DOWN:
      {
        if (c == PAGE_UP) {
          E.cy = E.rowoff;
        } else if (c == PAGE_DOWN) {
          E.cy = E.rowoff + E.screenrows - 1;
          if (E.cy > E.numrows) E.cy = E.numrows;
        }

        int times = E.screenrows;
        while (times--)
          editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
      }
      break;

    case ARROW_UP:
    case ARROW_DOWN:
    case ARROW_LEFT:
    case ARROW_RIGHT:
    case CTRL_ARROW_LEFT:
    case CTRL_ARROW_RIGHT:
      editorMoveCursor(c);
      break;

    case '\x1b':
      E.is_insert_mode = 0; /* go to normal */
      break;

    default:
      if (E.is_insert_mode) {
        editorInsertChar(c);
      } else {
        /* simple normal-mode motions: h/j/k/l, x, i, m, ' */
        if (c == 'h') editorMoveCursor(ARROW_LEFT);
        else if (c == 'l') editorMoveCursor(ARROW_RIGHT);
        else if (c == 'k') editorMoveCursor(ARROW_UP);
        else if (c == 'j') editorMoveCursor(ARROW_DOWN);
        else if (c == 'x') editorDelChar();
        else if (c == 'i') {
          if (E.read_only) {
            editorSetStatusMessage("Cannot enter insert mode in read-only mode");
          } else {
            E.is_insert_mode = 1;
          }
        }
        else if (c == 'w') {
          /* word motion forward in normal mode */
          editorMoveCursor(CTRL_ARROW_RIGHT);
        }
        else if (c == 'b') {
          /* word motion backward in normal mode */
          editorMoveCursor(CTRL_ARROW_LEFT);
        }
        else if (c == 'm') {
          /* set mark - wait for next character */
          editorSetStatusMessage("Mark: m_");
          editorRefreshScreen();
          int mark_char = editorReadKey();
          if (mark_char >= 'a' && mark_char <= 'z') {
            editorSetMark(mark_char);
          } else {
            editorSetStatusMessage("Invalid mark character");
          }
        }
        else if (c == '\'') {
          /* jump to mark - wait for next character */
          editorSetStatusMessage("Jump to mark: '_");
          editorRefreshScreen();
          int mark_char = editorReadKey();
          if (mark_char >= 'a' && mark_char <= 'z') {
            editorJumpToMark(mark_char);
          } else {
            editorSetStatusMessage("Invalid mark character");
          }
        }
      }
      break;
  }

  quit_times = FLUKE_QUIT_TIMES;
}

/* init */

void initEditor() {
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.rowoff = 0;
  E.coloff = 0;
  E.numrows = 0;
  E.row = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;
  E.syntax = NULL;
  E.is_insert_mode = 1;
  E.soft_wrap_enabled = 0;
  E.vrowoff = 0;
  E.show_line_numbers = 1;
  E.read_only = 0;

  /* clear marks */
  for (int i = 0; i < 26; i++) {
    E.marks[i].set = 0;
    E.marks[i].cy = 0;
    E.marks[i].cx = 0;
  }

  /* clear undo/redo stacks */
  for (int i = 0; i < g_undo_len; i++) freeSnapshot(g_undo_stack[i]);
  g_undo_len = 0;
  for (int i = 0; i < g_redo_len; i++) freeSnapshot(g_redo_stack[i]);
  g_redo_len = 0;

  if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
  E.screenrows -= 2;
  E.screencols -= FLUKE_LINE_NUMBER_WIDTH;
  
  /* load configuration */
  loadConfig();
}

int main(int argc, char *argv[]) {
  enableRawMode();
  initEditor();
  if (argc >= 2) {
    editorOpen(argv[1]);
  }

  editorSetStatusMessage(
    "HELP: Ctrl-S save | Ctrl-E saveas | Ctrl-Q quit | Ctrl-F find | Ctrl-G goto | Ctrl-W wrap | Ctrl-N lines | Ctrl-P cmd | Ctrl-O open | Ctrl-T theme | Ctrl-L readonly | Esc normal | i insert | m mark | ' jump | Ctrl-U undo | Ctrl-R redo");

  while (1) {
    editorRefreshScreen();
    editorAutosave(); /* periodic autosave */
    editorProcessKeypress();
  }

  return 0;
}