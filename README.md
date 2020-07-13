## introduction
A developing compiler based Java named "tins", which can compiler itself.

simple and powerful, without gc, vm... so it's fast.

## todo
已经将struct名字保存到符号表
接下来直接生成语法树，生成语法树
在解析语法树的阶段进行检查，如果符号表没有该条目，则报错
生成语法树时遇到func1(), func1(int a)，只记录这是个函数调用节点，在解析阶段再匹配函数符号表
### 1、Parser
- 数组赋值表达式
- 结构体赋值表达式

格式化输出，查看为什么b().c.d，只显示了b
以及c[i+9]，只显示了c

强制转换时，输出了太多层括号.