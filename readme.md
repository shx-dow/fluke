# Fluke Editor

> A lightweight, fast, and feature-rich text editor written in C

Fluke is a terminal-based text editor inspired by `kilo` but built to be a serious daily driver. It combines the simplicity of minimal editors with modern features like fuzzy search, command palette, themes, and intelligent editing.

## ✨ Features

### 🧱 Core Editing
- **Modal editing** - Insert and normal modes (like vim)
- **Multi-level undo/redo** - Full editing history with Ctrl+U/Ctrl+R
- **Smart auto-indent** - Maintains indentation on new lines
- **Smart backspace** - Dedent when appropriate
- **Word motion** - Ctrl+Left/Right for word-by-word navigation
- **Incremental search** - Live highlighting of all matches as you type

### 🎨 Visual & Themes
- **5 built-in themes** - Default, Dark, Light, Monokai, Solarized
- **Syntax highlighting** - C/C++ support with extensible system
- **Line numbers** - Toggle on/off, right-aligned
- **Soft word wrap** - Toggle for long lines
- **Status bar** - File info, cursor position, mode indicator

### 🚀 Navigation & Productivity
- **Command palette** (Ctrl+P) - Fuzzy search through all commands
- **File browser** (Ctrl+O) - Fuzzy file opening from current directory
- **Go to line** (Ctrl+G) - Jump directly to any line number
- **Mark system** - Set marks (ma) and jump to them ('a)
- **Persistent cursor** - Remembers position between sessions

### 🛡️ Reliability
- **Autosave & recovery** - Automatic backup every 30 seconds
- **Read-only mode** - Safe viewing of important files
- **Crash recovery** - Restore unsaved work after unexpected exit
- **Save-as functionality** - Easy file duplication

## �  Quick Start

### Build
```bash
make
```

### Usage
```bash
# Open a file
./fluke myfile.c

# Create new file
./fluke
```

## ⌨️ Key Bindings

### File Operations
| Key | Action |
|-----|--------|
| `Ctrl+S` | Save file |
| `Ctrl+E` | Save as (new filename) |
| `Ctrl+O` | Open file (fuzzy finder) |
| `Ctrl+Q` | Quit (with confirmation) |

### Editing
| Key | Action |
|-----|--------|
| `i` | Enter insert mode |
| `Esc` | Enter normal mode |
| `x` | Delete character (normal mode) |
| `Ctrl+Z` | Undo |
| `Ctrl+Y` | Redo |
| `Ctrl+F` | Find/search |
| `Ctrl+G` | Go to line |

### Navigation
| Key | Action |
|-----|--------|
| `Arrow keys` | Move cursor |
| `Ctrl+Left/Right` | Word motion |
| `Home/End` | Line start/end |
| `Page Up/Down` | Page navigation |
| `ma` | Set mark 'a' (normal mode) |
| `'a` | Jump to mark 'a' (normal mode) |

### Interface
| Key | Action |
|-----|--------|
| `Ctrl+P` | Command palette |
| `Ctrl+W` | Toggle word wrap |
| `Ctrl+L` | Toggle line numbers |
| `Ctrl+T` | Cycle themes |
| `Ctrl+R` | Toggle read-only mode |
| `Ctrl+H` | Show help |

## 🎨 Themes

Fluke comes with 5 carefully crafted themes:

- **Default** - Classic terminal colors
- **Dark** - Easy on the eyes for long coding sessions  
- **Light** - Clean and bright for daytime work
- **Monokai** - Popular dark theme with vibrant syntax colors
- **Solarized** - The beloved low-contrast theme

Switch themes with `Ctrl+T` or the `theme` command in the palette.

## 🔧 Configuration

Fluke automatically saves your preferences including:
- Last cursor position for each file
- Current theme selection
- Interface settings (line numbers, wrap mode)

Configuration is stored in your home directory and loaded automatically.

## 📋 Command Palette

Press `Ctrl+P` to open the command palette with fuzzy search:

- `save` / `write` - Save current file
- `saveas` - Save with new filename  
- `quit` / `quit!` - Exit editor
- `goto` - Jump to line number
- `find` - Search in file
- `open` - Open file browser
- `wrap` / `nowrap` - Toggle word wrap
- `lines` / `nolines` - Toggle line numbers
- `theme` - Cycle themes
- `marks` - Show all marks
- `readonly` - Toggle read-only mode
- `help` - Show keybinding help

## 🏗️ Architecture

Fluke is built with a clean, modular architecture:

- **Single file design** - Easy to build and distribute
- **Minimal dependencies** - Only standard C library
- **Memory efficient** - Optimized for large files
- **Cross-platform** - Works on Linux, macOS, and other Unix systems

## 🤝 Contributing

Fluke is designed to be hackable and extensible. The codebase is well-commented and structured for easy modification.

### Building from Source
```bash
git clone https://github.com/shx-dow/fluke.git
cd fluke
make
```

### Development
- C99 standard compliance
- No external dependencies beyond libc
- Comprehensive error handling
- Memory leak free

## 📊 Performance

- **Startup time**: < 10ms for most files
- **Memory usage**: ~1MB base + file size
- **File size limit**: Tested with files up to 100MB
- **Binary size**: < 100KB (optimized build)

## 🎯 Philosophy

Fluke aims to be:
- **Fast** - Instant startup, responsive editing
- **Minimal** - No bloat, essential features only
- **Reliable** - Never lose your work
- **Hackable** - Easy to understand and modify
- **Beautiful** - Clean interface, great themes

## 📜 License

MIT License - see LICENSE file for details.

## 🙏 Acknowledgments

- Inspired by the `kilo` editor tutorial
- Themes inspired by popular editor color schemes
- Built with love for terminal enthusiasts

---

*"Sometimes the best tools are the simplest ones, perfected."*