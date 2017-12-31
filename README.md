# company-mlton

[`company-mlton`](https://github.com/MatthewFluet/company-mlton) is a
[`company-mode`](http://company-mode.github.io/) completion back-end
for [MLton](http://mlton.org)/Standard ML.  It provides completion for
Standard ML keywords and for Standard ML (long) identifiers.
Candidate completion identifiers for the latter are loaded from a
basis file created by `mlton` using `-show-basis <file>` or
`(*#showBasis "<file>"*)`.  `company-mlton` ships with a default basis
file that corresponds to MLton's default environment (implicitly used
by `mlton` when compiling a `.sml` file).

## Screenshot

![company-mlton screenshot](screenshot.png)

## Installation

### Dependencies

 * Emacs packages
   * [`company-mode`](http://company-mode.github.io/) &gte; 0.9.4 (required)
   * [`dash`](https://github.com/magnars/dash.el) &gte; 2.12.0 (required)
   * [`sml-mode`](https://elpa.gnu.org/packages/sml-mode.html) (recommended)
 * [MLton](https://github.org/MLton/mlton) &gte; 20171229.155218-ga5d65b8

<!-- ### Install via [MELPA](https://melpa.org/) -->

### Install via Git

Clone repository:
``` shell
cd ~/.emacs.d
git clone https://github.com/MatthewFluet/company-mlton
```

Add to `.emacs`:
``` emacs-lisp
(add-to-list 'load-path "~/.emacs.d/company-mlton")
(require 'company-mlton)
(add-hook 'sml-mode-hook #'company-mlton-init)
```
