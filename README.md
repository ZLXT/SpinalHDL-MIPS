## 项目说明

这个项目是将《自己动手写CPU》(雷思磊)中的Verilog版本的源码翻译为SpinalHDL版本的源码。

### 项目目的

SpinalHDL入门时用来练习的项目比较少，我最近也在看《自己动手写CPU》这本书，因此就有了将书里边的源码翻译为SpinalHDL版本的念头。希望通过这种方式能提高自己的理解。

### 项目的配套文章

我会把文章发我的[博客](https://zlxt.gitee.io/)上。但是我不能保证文章能跟上代码的进度，而且本人文笔不太好，有些地方讲解可能不是很清晰。还是看代码比较实在点。

---

## 文件夹说明

文件夹与《自己动手写CPU》[源码](https://zlxt.lanzoui.com/iblobgd)中文件夹的名称对应。

其中NoramlVers存放的是调试正确的代码，仿真波形与书中的相同。

WaeningVers中存放的是有问题的代码，虽然可以进行编译，但仿真的波形不正确。

> 之所以把不正确的代码也放上来，是希望大家可以找下我犯的错误，来做一个参考和警戒。了解设计中的错误对提升自己的理解也是有很大帮助的。

## 运行环境

### 快速布置

如果想快速开始，可以使用[SpinalTemplateSbt](https://github.com/SpinalHDL/SpinalTemplateSbt)模板。

将`./src/main/scala/mylib`中原来的代码删除，并放入本项目源码即可。

例如学习Chapter2中的代码，mylib文件夹中的文件是：

> LoadRom.scala
>
> MyCpuSimVer01.scala
>
> MyCpuVer01.scala
>
> rom.data

代码运行需要修改的地方：

1. MyCpuVer__.scala中的`val patch`，修改为自己的路径
2. MyCpuSimVer__.scala中的`val patch`，修改为为自己的路径

### 具体配置

> SpinalHDL-1.4.0 (SpianlHDL的版本低一些也没关系，我暂时没用到太高级的功能)
>
> 仿真：Verilator
>
> 波形查看：GTKwave
