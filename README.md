# Readme #

- 这是Liu Xin的emacs配置文件，只支持emacs24以上版本，我用emacs主要进行C++开发和用org管理TODO，有时候也写点python。
- init-basic.el是基本的emacs配置，只需要emacs自身，不依赖第三方插件。
- init-custom.el是个人相关的自定义配置。
- init-elpa.el用于配置package.el管理的插件。
- init-lisp.el用于配置lisp目录下的第三方插件，及~/.emacs.d/lisp目录下的其它第三方插件，比如cedet，jdee，pymacs等。
- init-toolbar.el是我的工具栏配置。
- bin目录用来保存windows下的可执行文件，比如windows默认没有的grep等。
- elpa目录保存package.el安装的插件。
- lisp目录是第三方elisp插件。
- etc/images目录是toolbar用的图标文件。
- etc/sample是一些文件示例。
- etc/snippets目录是yasnippet的snippets定义。
- etc/templates目录是autoinsert模板。

# Install #

- git clone http://git.oschina.net/meteor1113/dotemacs.git
- 把etc/sample/.emacs拷贝到$HOME目录下，修改load-path为dotemacs的实际路径，根据需要可注释掉部分内容。
- 如果需要cedet，可以从 http://cedet.sourceforge.net 下载cedet并安装（比如安装到~/.emacs.d/lisp目录下）。
- 如果需要用jdee开发java，可以从 http://jdee.sourceforge.net 下载jdee并安装，安装位置可参考cedet。
- 如果需要用ropemacs开发python，可以分别从 http://rope.sourceforge.net ， http://pymacs.progiciels-bpi.ca/pymacs.html ， http://rope.sourceforge.net/ropemacs.html 下载rope，pymacs，ropemacs并安装，把pymacs编译时生成的pymacs.el文件安装到emacs里，安装位置可参考cedet。（注意ropemacs需要下载snapshot版，release的版本不能补全）

# Screenshot #

![](./etc/screenshots/cpp.png)

# Email #

- meteor1113@qq.com
