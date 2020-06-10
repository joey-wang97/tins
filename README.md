## introduction
A developing compiler based Java named "tins", which can compiler itself.

simple and powerful, without gc, vm... so it's fast.

## todo
#### lexer
#### DefParser
- 跳过强转表达式(括号表达式)
#### Parser
- 运算符改为贪婪模式，使用ignoreLineBreak获取下一个token
 ```
 while(a 
 && b)
```
- 数组长度表达式
- 数组赋值表达式
- 结构体赋值表达式