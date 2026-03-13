# Relatives

Это проект сделанный в рамках курса НИС кафедры ПиРВИС НИУ ВШЭ 2025.

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
