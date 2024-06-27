# Emacs config

## 适用

- Emacs 29.x
- Mac GUI
- Go/Python/React（Javascript） 等技术栈开发，以及 YAML/JSON/Markdown/Org 等纯本编辑

## UI

- 字体：Sarasa Term SC Nerd
- 主题：doom-one

## 安装 Emacs 和依赖

1、[emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)

``` sh
brew reinstall gcc libgccjit
brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --with-cacodemon-icon
ln -s /usr/local/opt/emacs-plus@29/Emacs.app /Applications
```

卸载 `brew uninstall emacs-plus@29`

2、Packages

打开 Emacs 时自动安装依赖包。我使用[中科大的源](https://mirrors.ustc.edu.cn/help/elpa.html)[中科大的源]]（清华源更新经常出问题，不建议用）。或者官方源 + 代理：

``` elisp
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ))

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "127.0.0.1:1087")
        ("https" . "127.0.0.1:1087")))
```

或者 `http_proxy=http://127.0.0.1:7890 emacs`。

如果遇到安装问题执行 `M-x package-refresh-content` 然后重启 Emacs。

3、二进制

- [ripgrep](https://github.com/BurntSushi/ripgrep) `brew install ripgrep`
- 编码，更多见下方 IDE：
  - 语法检测 flycheck [Supported languages](https://www.flycheck.org/en/latest/languages.html#flycheck-languages)
  - 统一使用 lsp，需要安装对应语言的 [Languages](https://emacs-lsp.github.io/lsp-mode/page/languages/)

4、安装字体：`brew install --cask font-sarasa-nerd` https://github.com/laishulu/Sarasa-Term-SC-Nerd

5、安装 nerd-icon：doom-mode-line 4.0.0 之后不再支持 all-the-icons 由 nerd-icons 代替：打开 [nerdfont](https://www.nerdfonts.com/#home) ，下载 Symbols Nerd Font 即可。

## 开发环境

### Go

按照 [官方说明](https://golang.org/doc/install) 安装 Go，LSP server 用的是 [gopls](https://github.com/golang/tools/tree/master/gopls)。

``` sh
GO111MODULE=on go install golang.org/x/tools/gopls@latest
```

lint 工具：

``` sh
curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.50.1
```

配置文件在：https://github.com/zhangjie2012/dotfiles/blob/master/_golangci.yaml

另外安装 [gomodifytags](https://github.com/fatih/gomodifytags)。

``` sh
go install github.com/fatih/gomodifytags@latest
```

### Python

lsp-server `python3 -m pip install 'python-lsp-server[all]'`

lint `python3 -m pip install flake8` 配置文件：https://github.com/zhangjie2012/dotfiles/blob/master/_flake8

format `python3 -m pip install black`

### React Web 开发

ESLint `npm install -g eslint`。

flycheck 配置 ESLint 经常出现各种奇奇怪怪的问题，从来没有一次性成功过，汇总的自查方法：

1. 全局安装 ESLint，我不使用项目中单独的配置
2. `(setq flycheck-javascript-eslint-executable "eslint")` 指定 eslint 路径
3. `flycheck-select-checker` 指定 ESLint
4. `flycheck-verify-setup` 查看二进制路径和配置文件是否生效
   + ESLint 全局配置文件在用户目录下，具体可以查看 ESLint 的文档，ESLint 一直更新可能会有变化
   + 我的配置在 https://github.com/zhangjie2012/dotfiles/blob/master/_eslintrc.json `ln -s dotfiles/_eslintrc.json .eslintrc.json` 添加软连接
5. 以上 Emacs 都没问题，但是检测不符合预期，要检查下用的是哪里的配置文件，以及配置文件是否有问题
   + `eslint --print-config file.js` 查看使用的配置文件是什么
   + `eslint file.js` 查看错误提示与 Emacs 是否相同
   + 看 eslint 报错，缺什么 **全局** 安装

核心思路是：先保证 eslint 本身运行没问题，再看 Emacs flycheck 配置是否正常。
