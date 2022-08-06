
#Stack based bookmarks for the Lazarus IDE.

This add-on adds a menu entry in the view main menu and other in the search main menu.

Go to main menu view StackBookmarks opvtion to view a dockable window.

You can assign shortcuts in the Tools->Options->Keymappings.

For example

- Push Ctrl K Space
- Pop Ctrl K Z
- Swap Ctrl K X
- First   Ctrl K Home
- Previous Ctrl K left
- Next Ctrl K right
- Last Ctrl K End
- View stack bookmarks window   Ctrl K V

**WARNING:  in beta state if occurs any error I recommend to erase the ProjectFileName.bkm file**

##Features.

Unlimited numbers of bookmarks.

Does not interfere with native IDE bookmarks.

Use bookmarks without having to remember the numbers assigned to each bookmark.

Descriptive bookmarks. You can see the line of code where the bookmark is or **edit** the description for the most used bookmarks.

Quickly drop (**Push**) a stack-based bookmark, move anywhere, then simply **Pop** the bookmark to go back where you were.

Easy to navigate between bookmarks (First, Previous,Next,Last) or if Shift key is pressed only navigate in the current editor.

Swapping Bookmarks
  The most recent bookmark position is changed with the current editor position
  The code editor moves to the old position of the most recent bookmark

  Use case. when you are moving between two alternative code locations, push a bookmark in first position and go to the second position,
  then you go to the first position **swapping** the bookmark  and return to the second position **swapping** the bookmark.

Insert bookmarks in the list.

Rearrange the bookmarks order using drag and drop in the bookmark list.

Edit the bookmark description. **F2**.

Lock the bookmark. The locked bookmarks are not deleted from the list when you **POP** a bookmark.
  (You can move the most Used bookmarks at the begin of the list and Lock the last most used, then the bookmarks will remain in the list)

Navigate only the bookmarks in the current editor if the shift key is pressed.
   Press **Enter** or **double click** on the list to go to the bookmark.

Import bookmarks  Add previous saved bookmarks to the list.

Export bookmarks  Saves the current bookmarks to a File.

The bookmarks are saved per project in file ProjectName.bkm  ( if you use GIT for version control you can add to .gitignore the extension .bkm)

Undo is not supported. If you delete or move a line with a bookmark and then Undo the changes the bookmark will remain in its new position.


#Screenshots

![screenshoot](https://user-images.githubusercontent.com/49276674/183255985-47353ea7-2998-4374-9e27-81e8cadbfca3.png)


![configure keys](https://user-images.githubusercontent.com/49276674/183255988-1dd342da-6408-47c1-bdff-214c67356535.png)

