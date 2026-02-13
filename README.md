# My Emacs Config (Emacs 30+)

> 本文档由 AI 生成。

这份配置专为 **Emacs 30.0+** 设计，充分利用了 Emacs 30 的原生特性（Tree-sitter, Native Compilation, IO 优化），打造了一个极速、现代化且稳定的开发环境。

## 核心特性

- **高性能**: 启用 Emacs 30 IO 优化 (`process-adaptive-read-buffering`)，移除冗余 UI 渲染。
- **Tree-sitter 全栈**: Go, Python, JS/TS/JSX, JSON, CSS, HTML 全面迁移至 `*-ts-mode`，配合 `treesit-auto` 自动管理语法文件。
- **异步格式化**: 使用 `Apheleia` 实现真正的后台格式化 (Prettier/Goimports)，无卡顿。
- **现代交互**: 
  - 补全: Company + LSP
  - 界面: Vertico + Consult + Marginalia
  - 滚动: Pixel Scroll Precision Mode

## 适用场景

- MacOS GUI (推荐) / Linux
- **Go**: `gopls` + Inlay Hints (参数提示)
- **Frontend**: `ts-ls` + Prettier (2空格强制)
- **Python**: `pylsp` + Black

## 安装要求

1. **Emacs 30.0+**
   ```sh
   brew tap d12frosted/emacs-plus
   brew install emacs-plus@30 --with-native-comp --with-poll --with-modern-icon
   ```

2. **外部依赖**
   ```sh
   # 基础工具
   brew install ripgrep

   # Language Servers & Formatters
   go install golang.org/x/tools/gopls@latest
   npm install -g typescript-language-server typescript prettier
   pip install "python-lsp-server[all]" flake8 black
   ```

## 配置详情

### Go 开发
- Mode: `go-ts-mode`
- 特性: 开启 Inlay Hints，保存自动 `goimports`。

### 前端开发 (React/TS)
- Mode: `js-ts-mode`, `tsx-ts-mode`
- 优化: 关闭类型推导提示以提升性能，强制 2 空格缩进。
- 格式化: 保存自动 `prettier`。

### Python 开发
- Mode: `python-ts-mode`
- 特性: 集成 `flake8` 检查，保存自动 `black`。

## 常用快捷键

| 功能 | 快捷键 | 说明 |
| :--- | :--- | :--- |
| **LSP** | `M-.` | 跳转定义 (Peek) |
| | `M-?` | 查找引用 |
| | `C-c l r` | 重命名 |
| | `C-c l a` | Code Actions |
| **Format** | `<f8> q` | 手动格式化 |
| **Fold** | `C-'` | 折叠/展开代码块 |

## 常见问题

1. **Tree-sitter 报错 "grammar unavailable"**
   首次打开文件时会自动提示安装，按 `y` 确认即可。或运行 `M-x treesit-install-language-grammar`。

2. **LSP 文档弹窗行为**
   配置为**手动触发**（鼠标悬停或快捷键），避免遮挡代码。

## Org Mode 备忘 (Legacy)

- 禁止 `_` 转义下标: `#+OPTIONS: ^:nil`
- 自动生成目录: 安装 `toc-org` 后使用 `:TOC:` 标签。
