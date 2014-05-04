## Readme ##

- 这是Meteor Liu的emacs配置文件，兼容Linux/Windows/MacOSX下的emacs-22以上版本，我用emacs主要进行C++开发和用org管理TODO，有时候也写点python。
- init-basic.el是基本的emacs配置，只需要emacs自身，不依赖第三方插件。
- init-misc.el用于配置lisp目录下的第三方插件。
- init-site.el用于配置不在lisp目录下的其它第三方插件，比如cedet，ecb，jdee，pymacs等，我一般把它存在site-lisp(Windows)或~/.emacs.d(Linux and MacOSX)目录下，主要是因为这些插件比较大，我不愿意把它存在git里。
- init-toolbar.el是我的工具栏配置。
- lisp目录是第三方elisp插件。
- site-lisp目录是本地elisp目录(未包含在版本库里，用于本地多操作系统共享)。
- bin目录用来存可执行文件，比如win32下默认没有的grep等。
- etc/snippets目录是yasnippet的snippets定义。
- etc/templates目录是autoinsert模板。
- etc/images目录是toolbar用的图标文件。
- etc/sample/.emacs是.emacs文件示例。
- etc/sample/proj.el是配置ede-cpp-root-project(参考cedet的ede)项目的示例文件。
- etc/sample/diary是diary文件的例子。
- etc/sample/site-start.el是portable emacs的例子，把这个文件拷贝到site-lisp目录就行。
- etc/sample/.dir-locals.el是.dir-locals.el的例子。

## Install ##

- git clone http://github.com/meteor1113/dotemacs.git
- 把etc/sample/.emacs拷贝到$HOME目录下，修改load-path为dotemacs的实际路径，根据需要可注释掉部分内容。
- 如果需要cedet，可以从 http://cedet.sourceforge.net 下载cedet并安装（Windows我一般直接解压在site-lisp目录下，Linux和MacOSX一般解压到~/.emacs.d目录下）。
- 如果需要ecb，可以从 http://ecb.sourceforge.net 下载ecb并安装，安装位置可参考cedet。
- 如果需要用jdee开发java，可以从 http://jdee.sourceforge.net 下载jdee并安装，安装位置可参考cedet。
- 如果需要用ropemacs开发python，可以分别从 http://rope.sourceforge.net ， http://pymacs.progiciels-bpi.ca/pymacs.html ， http://rope.sourceforge.net/ropemacs.html 下载rope，pymacs，ropemacs并安装，把pymacs编译时生成的pymacs.el文件放在site-lisp或~/.emacs.d目录下。（注意ropemacs需要下载snapshot版，release的版本不能补全）

Screenshot
----------

[[./etc/screenshots/cpp.png]]

Email
-----

- meteor1113@gmail.com
