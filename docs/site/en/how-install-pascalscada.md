##### Introduction

First of all, fell free to send corrections or alternative ways to this method 🙂

The main goal of this page is instruct how to install the latest PascalSCADA snapshot on the latest **Lazarus** version. Why I don’t explains how install it on Delphi or on a outdated Lazarus version, is question to another page, or feel free to send the instructions to install it on the desired IDE.

**Warning** : The current PascalSCADA package will install others packages together with PascalSCADA. The packages are:

  * ZeosLib 7.2
  * BGRABitmap
  * BGRAControls

So, if you dislike some of these packages, feel free to create a new package without the undesired package.

**BIG Warning 2:** The PascalSCADA isn’t like others SCADA systems. It’s a package, plugin or a add-on (you name it) for Lazarus to enable it to create HMI/SCADA applications. So, a wizard with Next, Next then finish buttons will be not available.

##### **What I need before start?**

  1. Latest Lazarus version. If you are a Windows user, you can get Lazarus from: <http://www.lazarus-ide.org/>. If you are a Linux/FreeBSD user, you can get it from your package system.
  2. Latest PascalSCADA: available on [this link](<http://www.pascalscada.com/download/>).
  3. WinRAR or something else that can decompress .bz2 files.

##### Installing…

Well, the first to be installed is Lazarus. I don’t want explain how to install Lazarus on any OS/platform that exists on this world, because install Lazarus is a very simple task, both on Windows (With wizard next, next and finish) or on Linux (Linux Mint 17.3 here: sudo apt-get install….). At the end of Lazarus install process, you should open it and see something similar to this:

[![Fresh Lazarus installation](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_034-300x168.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_034.png>)Fresh Lazarus installation

If you see your fresh Lazarus similar to this, you are on the right way. The next step is install the PascalSCADA package. Please forget the Lazarus by a moment. Go to your Downloads folder and find the downloaded PascalSCADA and extract it to a folder of your choice.

After extract your file, return to Lazarus and select the menu bellow:

[![Installing a new package on Lazarus IDE](http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-14-000414-300x170.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-14-000414.png>)Installing a new package on Lazarus IDE

Click on this menu. A open file dialog will be showed:

[![Find PascalSCADA on your file system](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_035-300x170.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_035.png>)Find PascalSCADA on your file system

Navigate to the folder where you extracted the PascalSCADA. You should see the following files:

  * pascalscada_common.lpk
  * pascalscada_full.lpk
  * pascalscada.lpk
  * pascalscada_db.lpk
  * pascalscada_hmi.lpk

If you don’t see one of these files, check your downloaded file, paths. If everything is ok, you should see a screen like this:

[![Find PascalSCADA on your file system \(complete view\)](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_036-300x168.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_036.png>)Find PascalSCADA on your file system (complete view)

Select the file “pascalscada_full.lpk” then open it. The package window of pascalscada_full file will be displayed:

[![PascalSCADA package opened](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_038-300x167.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_038.png>)PascalSCADA package opened

The last step, is install it into your IDE. To do this, click on button “Use &gt;&gt;” then select install.

[![Installing PascalSCADA on your Lazarus IDE](http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-14-002536-300x168.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-14-002536.png>)Installing PascalSCADA on your Lazarus IDE

￼

The package pascalscada_full is a metapackage, used only to make a complete install of PascalSCADA on your Lazarus. Because of this, you will be bored with some messages:

[![Metapackage: used to make easy the complete install of PascalSCADA](http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-14-003209-300x123.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Captura-de-tela-de-2016-07-14-003209.png>)Metapackage: used to make easy the complete install of PascalSCADA

Click on “Install it, I like the fat” to continue the install. The next screen display a list with all packages that will be installed into your IDE, as displayed below:

[![List of all packages that will be installed together with PascalSCADA](http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_040-300x169.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Seleção_040.png>)List of all packages that will be installed together with PascalSCADA

More detailed view:

[![List of all packages that will be installed together with PascalSCADA](http://www.pascalscada.com/wp-content/uploads/2016/07/Automatically-installed-packages_041-300x204.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Automatically-installed-packages_041.png>)List of all packages that will be installed together with PascalSCADA

Click on OK to continue the install and get the final question, about the IDE recompile process, as you can see below:

[![Rebuild Lazarus confirmation](http://www.pascalscada.com/wp-content/uploads/2016/07/Rebuild-Lazarus_042-300x102.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Rebuild-Lazarus_042.png>)Rebuild Lazarus confirmation

To continue, click on “Yes”. The installation process will be started and your IDE will be recompiled. At the end of recompile process, the IDE will restarted. **If you have a project with unsaved changes, take care!!**

After the restart, depending of your PascalSCADA version, you should see the following component palete on your Lazarus:

[![PascalSCADA installed: communication port components palette](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_043-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_043.png>)PascalSCADA installed: communication port components palette [![PascalSCADA installed: communication protocol components palette](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_044-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_044.png>)PascalSCADA installed: communication protocol components palette [![PascalSCADA installed: utility components palette](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_045-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_045.png>)PascalSCADA installed: utility components palette [![PascalSCADA installed: tags palette](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_046-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_046.png>)PascalSCADA installed: tags palette [![PascalSCADA installed: screen controls palette](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_047-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_047.png>)PascalSCADA installed: screen controls palette [![PascalSCADA installed: user management and security components palette](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_048-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_048.png>)PascalSCADA installed: user management and security components palette [![PascalSCADA installed: database components palette](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_049-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_049.png>)PascalSCADA installed: database components palette [![PascalSCADA installed: FreePascal components palette](http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_050-300x27.png)](<http://www.pascalscada.com/wp-content/uploads/2016/07/Lazarus-IDE-v1.6-project1_050.png>)PascalSCADA installed: FreePascal components palette

If you see these tabs, congrats, you successfully installed the PascalSCADA on your Lazarus IDE.
