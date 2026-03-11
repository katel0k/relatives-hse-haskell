# Relatives

## Запуск из интерпретатора

```sh
cabal repl rel-lib
```

```hs
import Interpreter
import RuleUtils
import CommonRules
import Rules

let rules = CommonRules.commonRules
let test = Interpreter.findRelativesInFile "test.txt" rules
do x <- test "A"; Interpreter.prettyPrint x
```
