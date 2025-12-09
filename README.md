# Deriving Compiler

## 思路

从最开始，我想把论文中的三合一拆开，先 stackify，再 cps，最后 d，每一步只使用上一步的结果。
在做的过程中，我发现存在，上一步的结果不一定可能隐藏了一些内部结构，所以最后也只是先 s，再 s+k，最后 s+k+d，到和论文里一样的结果。
但是这个过程，还是给了我很多新的想法的，比如 stack 可以用来存 handler，有一种 continuation 的感觉；对 stackified 的解释器，只 k 正常的分支，让异常分支通过一个外部函数来操作，这个外部函数的定义确实可以在 calculating 的过程中得到。
最后 s+k+d 而不是 s、k、d 还有好处是没一步 calculate 出来的定义是不一样的，与其计算出来转换，可能直接计算一个新的更容易。

## Formatting

To check if files are already formatted (useful on CI):

```bash
$ fourmolu --mode check .
```

Find all the source files in a project with `git ls-files` and then use `fourmulu` to format those files:

```bash
$ fourmolu --mode inplace $(git ls-files '*.hs')
# Or to avoid hitting command line length limits and enable parallelism (12-way here):
$ git ls-files -z '*.hs' | xargs -P 12 -0 fourmolu --mode inplace
```
