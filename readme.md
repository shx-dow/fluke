# Fluke – A Minimal Text Editor

Fluke is a small, lightweight text editor written in C, inspired by [kilo](https://viewsourcecode.org/snaptoken/kilo/).
It’s designed to be **simple, fast, and easy to understand**, both for users who want a clean editing experience and for developers who want to dive into the codebase.

---

## ✨ Features

* Minimal codebase (\~1K lines of C, excluding comments)
* Syntax highlighting for multiple languages
* Search functionality within files
* Line numbering
* Lightweight — no external dependencies
* Cross-platform (Linux, macOS, WSL)

---

## 🚀 Installation

### Clone & Build

```bash
git clone https://github.com/shx-dow/fluke.git
cd fluke
make
```

### Run the Editor

```bash
./fluke filename.txt
```

---

## 🛠 Usage

| Keybinding        | Action      |
| ----------------- | ----------- |
| `CTRL-S`          | Save file   |
| `CTRL-Q`          | Quit editor |
| `CTRL-F`          | Search      |
| Arrow Keys        | Move cursor |
| Page Up/Page Down | Scroll      |

---

## 📂 Project Structure

```
fluke/
 ├── fluke.c        # Main source file
 ├── Makefile       # Build instructions
 └── README.md      # This file
```

---

## 📖 Inspiration

Fluke is heavily inspired by [kilo](https://viewsourcecode.org/snaptoken/kilo/) by Salvatore Sanfilippo, but with personal modifications, bug fixes, and enhancements to make it more practical for my workflow.

---

## 🤝 Contributing

Contributions are welcome!
If you have ideas for improvements or find a bug, open an issue or submit a pull request.

---
