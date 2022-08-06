
#Stack based bookmarks for the Lazarus IDE.

This add-on adds a menu entry in the view main menu and other in the search main menu.

You can assign shortcuts in the options shortcuts.
for example

- Push Ctrl K Space
- Pop Ctrl K Z
- Swap Ctrl K X
- First   Ctrl K Home
- Previous Ctrl K left
- Next Ctrl K right
- Last Ctrl K End
- View stack bookmarks window   Ctrl K V

**WARNING:  in beta state  if occurs any error I recommend to erase the ProjectFileName.bkm file**

##Features.

Unlimited numbers of bookmarks.

Does not interfere with native IDE bookmarks.

Use bookmarks without memorising numbers.

Descriptive bookmarks. You can see the line of code where the bookmark is or edit the description for the most used bookmarks.

Quickly drop (**Push**) a stack-based bookmark, move anywhere, then simply **Pop** the bookmark to go back where you were.

Easy to navigate between bookmarks.

Swapping Bookmarks
  The most recent bookmark position is changed with the current editor position
  The code editor moves to the old position of the most recent bookmark

  Use case. when you are moving between two alternative code locations, push a bookmark in one position and go to the two position,
  then you go to the one position swapping the bookmark  and return to the two position swapping the bookmark.

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


#Screenshots


