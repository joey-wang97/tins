## introduction
A developing compiler based Java11 named "tins"

## todo list

### 语义分析
- 变量重复定义
- struct之间的二元运算
- 测试所有语句if, for, while
- for语句的init只能为变量定义或普通表达式(不能为if, for等)
- 检查函数类型是否为正确类型，只能为关键字或struct名
- 检查赋值表达式类型是否合法
- 检查表达式操作，加法之外的二元运算符，不能操作string
- return，continue后面不能跟语句
- 表达式作为语句时，必须为AssignExpr或CallFuncExpr


## future todo
- switch