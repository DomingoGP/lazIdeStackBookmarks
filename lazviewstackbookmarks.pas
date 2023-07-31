{ Stack based bookmarks add-on for the Lazarus IDE

  Copyright (C) 2022 Domingo Galmés      dgalmesp@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{ Toolbar images taken from Lazarus LCL.

}


unit LazViewStackBookmarks;

//TODO: Assign key shortcuts. Ctrl+K Space  Ctrl+K Z  Ctrl+K X  Ctrl+K Home Ctrl+K End  Up Down
//TODO: Intercept???  Project Save As... and update folders.

//{$define TESTING}  //defined in TestProject1 project settings for debugging.
// and testing without using the lazarus IDE. Easy to debug.

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType,
  Buttons, ComCtrls, SynEdit, LazSynEditText, laz.VirtualTrees, ActiveX,
  IDECommands, IDEWindowIntf, LazIDEIntf, SrcEditorIntf, MenuIntf;

type

  { TLazViewStackBookmarks }
  TLazViewStackBookmarks = class(TForm)
    BookmarksTree: TLazVirtualStringTree;
    btnLock: TSpeedButton;
    btnPop: TSpeedButton;
    btnPush: TSpeedButton;
    btnSwap: TSpeedButton;
    Images:TImageList;
    tbbClear:TToolButton;
    tbbDelete:TToolButton;
    tbbEdit:TToolButton;
    tbbExport:TToolButton;
    tbbFirst:TToolButton;
    tbbImport:TToolButton;
    tbbInsert:TToolButton;
    tbbLast:TToolButton;
    tbbNext:TToolButton;
    tbbPrevious:TToolButton;
    tbbRefresh:TToolButton;
    ToolBar:TToolBar;
    procedure BookmarksTreeDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: boolean);
    procedure BookmarksTreeDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint;
      var Effect: longword; Mode: TDropMode);
    procedure BookmarksTreeDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: longword; var Accept: boolean);
    procedure BookmarksTreeGetHint(Sender:TBaseVirtualTree; Node:PVirtualNode;
      Column:TColumnIndex; var LineBreakStyle:TVTTooltipLineBreakStyle;
      var HintText:String);
    procedure BookmarksTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure BookmarksTreeKeyAction(Sender: TBaseVirtualTree; var CharCode: word;
      var Shift: TShiftState; var DoDefault: boolean);
    procedure BookmarksTreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: string);
    procedure BookmarksTreeNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
    procedure BookmarksTreeAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    //procedure BookmarksTreeBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
    //  Node: PVirtualNode; const ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure BookmarksTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellText: string; const CellRect: TRect;
      var DefaultDraw: boolean);
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnExportClick(Sender:TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnImportClick(Sender:TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnLockClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPopClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnPushClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnSwapClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender:TObject);
  private
    FIgnoreEvents: boolean;
    procedure DeleteTreeNodeNoEvents(Node: PVirtualNode; Reindex: boolean = True);
    procedure FillTreeView;
    procedure SelectTreeNodeNoEvents(aNode: PVirtualNode);
    procedure SetToolBarHorizontal;
    procedure SetToolBarVertical;
  end;

procedure Register;

{$ifdef TESTING}
procedure DoChangeActiveEditor(aNewEditor: TSynEdit; const aFileName: string;const aUnitName:string='');
function GetNormalizedFileName(const aBaseFolder:string;const aFileName:string;out aIsRelative:boolean):string;
{$endif}

var
  frmViewStackBM: TLazViewStackBookmarks;
  lazStackBookmarksCreator: TIDEWindowCreator; // set by Register procedure

resourcestring
  rsStackBookmarks = 'StackBookmarks';
  rsBookmarkDesc   = 'Bookmark description';
  rsBookmarkPush   = 'Push';
  rsBookmarkPop    = 'Pop';
  rsBookmarkSwap   = 'Swap';
  rsBookmarkLock   = 'Lock';

  rsBtnFirstHint   = 'Go to first bookmark. If Shift only in current file';
  rsBtnPreviousHint = 'Go to previous bookmark. If Shift only in current file';
  rsBtnNextHint    = 'Go to next bookmark. If Shift only in current file';
  rsBtnLastHint    = 'Go to last bookmark. If Shift only in current file';
  rsBtnPushHint    = 'Push the bookmark';
  rsBtnPopHint     = 'Go to last bookmark and delete the bookmark. If Shift only in current file';
  rsBtnSwapHint    = 'Go to last bookmark and bookmark the current position. If Shift only in current file';
  rsBtnInsertHint  = 'Inserts the bookmark in the list';
  rsBtnDeleteHint  = 'Deletes the bookmark from the list';
  rsBtnLockHint    = 'Lock/Unlock the bookmark. Locked bookmarks aren''t deleted by Pop';
  rsBtnEditHint    = 'Edit the bookmark description';
  rsBtnClearHint   = 'Deletes all bookmaks. If Shift only in current editor';
  rsBtnRefreshHint = 'Refreshes the bookmarks description (only on bookmarks in current editor';
  rsBtnImportHint  = 'Import bookmarks from file';
  rsBtnExportHint  = 'Save bookmarks to file';
  rsImpExpFilter   = 'Files *.bkm|*.bkm|All files *.*|*.*';
  rsImportTitle    = 'Import bookmarks';
  rsExportTitle    = 'Export bookmarks';

  rsSBMenuCaption = 'Stack Bookmarks';

  rsSB_CATEGORY_IDECMD ='Stack Bookmarks';
  rsSB_MENU           = 'Stack Bookmarks';
  rsSB_PUSH_MENU      = 'Push';
  rsSB_PUSH_IDECMD    = 'Push stack bookmark';
  rsSB_POP_MENU       = 'Pop';
  rsSB_POP_IDECMD     = 'Pop stack bookmark';
  rsSB_SWAP_MENU      = 'Swap';
  rsSB_SWAP_IDECMD    = 'Swap last stack bookmark';
  rsSB_FIRST_MENU     = 'First';
  rsSB_FIRST_IDECMD   = 'go to first stack bookmark';
  rsSB_LAST_MENU      = 'Last';
  rsSB_LAST_IDECMD    = 'go to last stack bookmark';
  rsSB_PREVIOUS_MENU   = 'Previous';
  rsSB_PREVIOUS_IDECMD = 'go to previous stack bookmark';
  rsSB_NEXT_MENU      = 'Next';
  rsSB_NEXT_IDECMD    = 'go to next stack bookmark';

  rsSB_C_POP_MENU       = 'Pop current editor';
  rsSB_C_POP_IDECMD     = 'Pop stack bookmark only in current editor';
  rsSB_C_SWAP_MENU      = 'Swap current editor';
  rsSB_C_SWAP_IDECMD    = 'Swap last stack bookmark only in current editor';
  rsSB_C_FIRST_MENU     = 'First current editor';
  rsSB_C_FIRST_IDECMD   = 'go to first stack bookmark only in current editor';
  rsSB_C_LAST_MENU      = 'Last current editor';
  rsSB_C_LAST_IDECMD    = 'go to last stack bookmark only in current editor';
  rsSB_C_PREVIOUS_MENU   = 'Previous current editor';
  rsSB_C_PREVIOUS_IDECMD = 'go to previous stack bookmark only in current editor';
  rsSB_C_NEXT_MENU      = 'Next current editor';
  rsSB_C_NEXT_IDECMD    = 'go to next stack bookmark only in current editor';

implementation

{$R *.lfm}

uses
  crc, laz2_DOM, laz2_XMLWrite, laz2_XMLRead,LCLIntf,
  LazLoggerBase, projectintf,LazFileUtils;

const
  FORM_NAME='LazViewStackBookmarks1'; //MUST not be equal to design time form name.

  DESCRIPTION_LEN = 80;
  SHIFTKEY_CODE = ssShift;    //ssCtrl, ssShift, ssAlt ...
  //ALTERNATE_ITEM_COLOR = $ebfbfc;

type

  TCaptureIdeEvents = class
  private
    FEnabled: boolean;
    procedure DoFirst(Sender:TObject);
    procedure DoFirstCurrent(Sender:TObject);
    procedure DoLast(Sender:TObject);
    procedure DoLastCurrent(Sender:TObject);
    procedure DoNext(Sender:TObject);
    procedure DoNextCurrent(Sender:TObject);
    procedure DoPop(Sender:TObject);
    procedure DoPopCurrent(Sender:TObject);
    procedure DoPrevious(Sender:TObject);
    procedure DoPreviousCurrent(Sender:TObject);
    procedure DoPush(Sender:TObject);
    procedure DoSwap(Sender:TObject);
    procedure DoSwapCurrent(Sender:TObject);
    procedure EditorAddChangeHandler;
    procedure EditorRemoveChangeHandler;
    procedure OnActiveEditorChanged(Sender: TObject);
    function OnFileSaved(Sender: TObject; aFile: TLazProjectFile; SaveStep: TSaveEditorFileStep; TargetFilename: string): TModalResult;
    procedure OnLineCountChanged(Sender: TSynEditStrings; aIndex, aCount: integer);
    function OnProjectClose(Sender: TObject;AProject: TLazProject): TModalResult;
    function OnProjectOpened(Sender: TObject;AProject: TLazProject): TModalResult;
    procedure OnIdeClose(Sender: TObject);
  end;


  PStackBookmark = ^TStackBookmark;

  { TStackBookmark }

  TStackBookmark = class
    XY: TPoint;
    TopLine: integer;
    Description: string;
    UnitnameSB:string;    //"Unitname" is a member of TObject. Error duplicate field.
    Filename: string;
    FilenameHash: uint32;
    TreeNode: PVirtualNode;
    FilenameIsRelative:boolean;
    Edited: boolean;  // when we edited description.
    Locked: boolean;  // don't pop locked bookmarks from stack
  private
    procedure GetData;
    procedure GetDescription;
  public
    constructor Create;
    function Clone: TStackBookmark;
    function InCurrentEditor: boolean;
  end;

var
  gBookmarks: TFPList;
  gBookmarksItemIndex: integer;
  gBookmarksInCurrentEditorCount: integer;
  gBookmarksCurrentEditorUnitname: string;
  gBookmarksCurrentEditorFilename: string;
  gBookmarksCurrentEditorFilenameIsRelative:boolean;
  gBookmarksCurrentEditorFilenameHash: uint32;
  gBookmarksCurrentEditor: TSynEdit;
  gCaptureIdeEventsObject: TCaptureIdeEvents;
  gBookMarksProjectFolder: string;     //Warning: I can't detect change on Project Save As


  function GetNormalizedFileName(const aBaseFolder:string;const aFileName:string;out aIsRelative:boolean):string;
  var
    wFN,wFP:string;
  begin
    wFN:=ExtractFileName(aFileName);
    //native directory separator
    wFP:=SwitchPathDelims(ExtractFilePath(aFileName),pdsSystem);
    // relative file name
    result:=ExtractRelativePath(aBaseFolder,wFP);
    aIsRelative := result<>wFP;
    result:=SwitchPathDelims(result,pdsUnix);
    //only saves as relatives if are childs of base path.
    if Pos('../',result)=1 then
    begin
      result:=SwitchPathDelims(wFP,pdsUnix);
      aIsRelative:=false;
    end;
    result:=result+wFN;
  end;

function HashString(const aStr: string): uint32;
var
  wLen: integer;
begin
  wLen := Length(aStr);
  Result := CRC32(0, nil, wLen);
  if wLen > 0 then
    Result := CRC32(Result, pbyte(@aStr[1]), wLen);
end;

procedure ClearBookmarksList;
var
  wI: integer;
  wO: TStackBookmark;
begin
  if frmViewStackBM <> nil then
    frmViewStackBM.BookmarksTree.Clear;
  if gBookmarks=nil then
    Exit;
  wI:=gBookmarks.Count-1;
  while wI>=0 do
  begin
    wO := TStackBookmark(gBookmarks.Items[wI]);
    wO.Free;
    gBookmarks.Delete(wI);
    Dec(wI);
  end;
  gBookmarksItemIndex := -1;
  gBookmarksInCurrentEditorCount := 0;
end;

procedure ClearBookmarksListOnlyCurrentEditor;
var
  wI: integer;
  wO: TStackBookmark;
begin
  if frmViewStackBM <> nil then
    frmViewStackBM.BookmarksTree.BeginUpdate;
  wI:=gBookmarks.Count-1;
  while wI>=0 do
  begin
    wO := TStackBookmark(gBookmarks.Items[wI]);
    if wO.InCurrentEditor then
    begin
      if (frmViewStackBM <> nil) and (wO.TreeNode<>nil) then
        frmViewStackBM.BookmarksTree.DeleteNode(wO.TreeNode,True);
      gBookmarks.Delete(wI);
      wO.Free;
      //if wI<gBookmarksItemIndex then
      //  Dec(gBookmarksItemIndex);
    end;
    Dec(wI);
  end;
  gBookmarksItemIndex:=gBookmarks.Count-1;
  if frmViewStackBM <> nil then
  begin
    frmViewStackBM.BookmarksTree.EndUpdate;
    if gBookmarksItemIndex>=0 then
      frmViewStackBM.SelectTreeNodeNoEvents(TStackBookmark(gBookmarks.Items[gBookmarksItemIndex]).TreeNode);
  end;
  gBookmarksInCurrentEditorCount := 0;
end;

procedure ClearBookmarksNodePointers;
var
  wI: integer;
begin
  wI:=gBookmarks.Count-1;
  while wI >= 0 do
  begin
    TStackBookmark(gBookmarks.Items[wI]).TreeNode := nil;
    Dec(wI);
  end;
end;

function CountBookmarksInCurrentEditor: integer;
var
  wI: integer;
  wBD: TStackBookmark;
begin
  Result := 0;
  wI := 0;
  while wI < gBookmarks.Count do
  begin
    wBD := TStackBookmark(gBookmarks.Items[wI]);
    if wBD.InCurrentEditor then
      Inc(Result);
    Inc(wI);
  end;
end;

procedure DoChangeActiveEditor(aNewEditor: TSynEdit; const aFileName: string;const aUnitName:string);
begin
  if gCaptureIdeEventsObject=nil then
    Exit;
  gCaptureIdeEventsObject.EditorRemoveChangeHandler;
  if gCaptureIdeEventsObject.FEnabled then
  begin
    gBookmarksCurrentEditor := aNewEditor;
    gBookmarksCurrentEditorFilename := GetNormalizedFileName(gBookMarksProjectFolder,aFileName,gBookmarksCurrentEditorFilenameIsRelative);
    gBookmarksCurrentEditorUnitname := aUnitName;
    gBookmarksCurrentEditorFilenameHash := HashString(gBookmarksCurrentEditorFilename);
    gBookmarksInCurrentEditorCount:=CountBookmarksInCurrentEditor;
    gCaptureIdeEventsObject.EditorAddChangeHandler;
  end
  else
    gBookmarksCurrentEditor := nil;
  if frmViewStackBM<>nil then
    frmViewStackBM.BookmarksTree.Invalidate;
end;

procedure ReturnFocusToEditor;
var
  wSourceEditor:TSourceEditorInterface;
  wSynEdit:TSynEdit;
begin
  {$ifdef TESTING}
    if gBookmarksCurrentEditor<>nil then
      gBookmarksCurrentEditor.SetFocus;
    Exit;
  {$endif}
  wSourceEditor := TSourceEditorInterface(SourceEditorManagerIntf.ActiveEditor);
  if (wSourceEditor <> nil) and (wSourceEditor.EditorControl is TSynEdit) then
  begin
    if IDETabMaster<>nil then
      IDETabMaster.ShowCode(wSourceEditor);
    wSynEdit := TSynEdit(wSourceEditor.EditorControl);
    if wSynEdit.CanSetFocus then
      wSynEdit.SetFocus;
  end;
end;

function GetActiveEditorData(var aFilename:string;var aUnitName:string;out aIsRelative:boolean):boolean;
var
  wSourceEditor:TSourceEditorInterface;
begin
  result:=false;
  {$ifdef TESTING}
    Exit;
  {$endif}
  wSourceEditor := TSourceEditorInterface(SourceEditorManagerIntf.ActiveEditor);
  if wSourceEditor <> nil then
  begin
    aFilename:=GetNormalizedFileName(gBookMarksProjectFolder, wSourceEditor.Filename,aIsRelative);
    aUnitName:=wSourceEditor.PageName;
    result:=true;
  end;
end;

procedure SetActiveEditor;
var
  wSourceEditor:TSourceEditorInterface;
  wSynEdit:TSynEdit;
begin
  {$ifdef TESTING}
    Exit;
  {$endif}
  wSourceEditor := TSourceEditorInterface(SourceEditorManagerIntf.ActiveEditor);
  // Change fired 3 times for one tab  Code,Form,Anchors???
  if (wSourceEditor <> nil) and (wSourceEditor.EditorControl is TSynEdit) then
  begin
    wSynEdit := TSynEdit(wSourceEditor.EditorControl);
    if wSynEdit<>gBookmarksCurrentEditor then
      DoChangeActiveEditor(wSynEdit,wSourceEditor.Filename,wSourceEditor.PageName);
  end;
end;

function GetActualFont(ctl: TControl): TFont;
begin
  Result := ctl.Font;
  while (Result.Name = 'default') and not (ctl is TForm) do begin
    ctl := ctl.Parent;
    Result := ctl.Font;
  end;
  if Result.Name = 'default' then Result := Screen.SystemFont;
end;

{$ifdef TESTING}
procedure GotoBookmark(aBD: TStackBookmark); overload;
begin
  if aBD<>nil then
  begin
    if aBD.InCurrentEditor and (gBookmarksCurrentEditor<>nil) then
    begin
      gBookmarksCurrentEditor.TopLine := aBD.TopLine;
      gBookmarksCurrentEditor.CaretXY := aBD.XY;
    end;
  end;
  ReturnFocusToEditor;
end;

{$else}

procedure GotoBookmark(aBD: TStackBookmark); overload;
var
  wSEI:TSourceEditorInterface;
  wFileName:string;
begin
  if aBD<>nil then
  begin
    if aBD.InCurrentEditor and (gBookmarksCurrentEditor<>nil) then //bookmark in current tab.
    begin
      gBookmarksCurrentEditor.TopLine := aBD.TopLine;
      gBookmarksCurrentEditor.CaretXY := aBD.XY;
    end
    else
    begin  // bookmark in other tab
      wFileName:=aBD.Filename;
      if aBD.FilenameIsRelative then
        wFileName:=gBookMarksProjectFolder+wFileName;
      wSEI:=SourceEditorManagerIntf.SourceEditorIntfWithFilename(wFilename);
      if wSEI<>nil then
      begin
       // SourceEditorManagerIntf.ActiveSourceWindow:=wSEI.;
        SourceEditorManagerIntf.ActiveEditor:=wSEI;
        wSEI.TopLine:=aBD.TopLine;
        wSEI.CursorTextXY:=aBD.XY;
      end
      else  //file not open
      begin
        LazarusIDE.DoOpenFileAndJumpToPos(wFilename,aBD.XY,aBD.TopLine,aBD.TopLine,aBD.TopLine,-1,-1,[]);
      end;
    end;
  end;
  ReturnFocusToEditor;
end;
{$endif}

procedure GoToBookmark(aIndex: integer); overload;
var
  wBD: TStackBookmark;
begin
  wBD := nil;
  if (aIndex >= 0) and (aIndex < gBookmarks.Count) then
    wBD := TStackBookmark(gBookmarks.Items[aIndex]);
  GoToBookmark(wBD);
end;

procedure GoToCurrentBookmark;inline;
begin
  GoToBookmark(gBookmarksItemIndex);
end;

// Updates the position of the bookmarks if lines are added or deleted in source file.
procedure TCaptureIdeEvents.OnLineCountChanged(Sender: TSynEditStrings; aIndex, aCount: integer);
var
  wI, wAdd: integer;
  wBD: TStackBookmark;
begin
  if gCaptureIdeEventsObject=nil then
    Exit;
  if (aCount = 0) or (gBookmarksInCurrentEditorCount <= 0) or (gBookmarks.Count <= 0) then
    Exit;
  wI:=0;
  while wI < gBookmarks.Count do
  begin
    wBD := TStackBookmark(gBookmarks.Items[wI]);
    if wBD.InCurrentEditor then
    begin
      if wBD.XY.Y >= aIndex then  //lines added or deleted before bookmark position.
      begin
        wAdd := aCount;
        if (aCount < 0) and (wBD.XY.Y <= (aIndex - aCount)) then  //current bookmark line deleted.
          wAdd := aIndex - wBD.XY.Y;            //new bookmark position= aIndex.
        wBD.XY.Y := wBD.XY.Y + wAdd;
        wBD.TopLine := wBD.TopLine + wAdd;
      end;
    end;
    Inc(wI);
  end;
  if frmViewStackBM <> nil then
    frmViewStackBM.BookmarksTree.Invalidate;
end;

constructor TStackBookmark.Create;
begin
  inherited;
  XY.X:=0;
  XY.Y:=0;
  TopLine:=0;
  Edited:=false;
  Description:='';
  UnitnameSB:='';
  Filename:='';
  FilenameIsRelative:=false;
  FilenameHash:=0;
  TreeNode:=nil;
end;

function TStackBookmark.Clone: TStackBookmark;
begin
  Result := TStackBookmark.Create;
  Result.XY := XY;
  Result.TopLine := TopLine;
  Result.FilenameHash := FilenameHash;
  Result.Filename := Filename;
  Result.FilenameIsRelative := FilenameIsRelative;
  Result.Description := Description;
  Result.UnitnameSB := UnitnameSB;
  Result.Edited := Edited;
  Result.TreeNode := TreeNode;
end;

function TStackBookmark.InCurrentEditor: boolean;
begin
  Result := FilenameHash = gBookmarksCurrentEditorFilenameHash;
end;

procedure TStackBookmark.GetDescription;
begin
  Description := Copy(gBookmarksCurrentEditor.Lines[XY.Y - 1], 1, DESCRIPTION_LEN);
end;

procedure TStackBookmark.GetData;
begin
  if gBookmarksCurrentEditor = nil then
    Exit;
  XY := gBookmarksCurrentEditor.CaretXY;
  TopLine := gBookmarksCurrentEditor.TopLine;
  Filename := gBookmarksCurrentEditorFilename;
  FilenameIsRelative:=gBookmarksCurrentEditorFilenameIsRelative;
  FilenameHash := gBookmarksCurrentEditorFilenameHash;
  UnitnameSB:= gBookmarksCurrentEditorUnitname;
  GetDescription;
end;

procedure SelectBookmark(aItemIndex: integer);
var
  wNode:PVirtualNode;
begin
  if aItemIndex<gBookmarks.Count then
    gBookmarksItemIndex := aItemIndex
  else
    gBookmarksItemIndex := gBookmarks.Count-1;
  if gBookmarksItemIndex>=0 then
  begin
    wNode:=TStackBookmark(gBookmarks.Items[gBookmarksItemIndex]).TreeNode;
    if (frmViewStackBM <> nil) and  (wNode <> nil) then
      frmViewStackBM.SelectTreeNodeNoEvents(wNode);
  end;
end;

procedure DeleteTreeNodeNoEventsStatic(aNode: PVirtualNode; Reindex: boolean);
begin
  if (frmViewStackBM = nil) or (aNode = nil) then
    Exit;
  frmViewStackBM.DeleteTreeNodeNoEvents(aNode, Reindex);
end;

procedure TLazViewStackBookmarks.SelectTreeNodeNoEvents(aNode: PVirtualNode);
var
  wOldIgnoreEvents: boolean;
begin
  wOldIgnoreEvents := FIgnoreEvents;
  FIgnoreEvents := True;
  try
    BookmarksTree.ClearSelection;
    BookmarksTree.AddToSelection(aNode);
    BookmarksTree.ScrollIntoView(aNode, false);
  finally
    FIgnoreEvents := wOldIgnoreEvents;
  end;
end;

procedure TLazViewStackBookmarks.DeleteTreeNodeNoEvents(Node: PVirtualNode; Reindex: boolean);
var
  wOldIgnoreEvents: boolean;
begin
  wOldIgnoreEvents := FIgnoreEvents;
  FIgnoreEvents := True;
  try
    BookmarksTree.DeleteNode(Node, Reindex);
  finally
    FIgnoreEvents := wOldIgnoreEvents;
  end;
end;

procedure TLazViewStackBookmarks.FillTreeView;
var
  wI: integer;
  wNode, wSelected: PVirtualNode;
  wBD: TStackBookmark;
begin
  if gBookmarks.Count<=0 then
    Exit;
  wSelected := nil;
  wNode:=nil;
  BookmarksTree.BeginUpdate;
  wI:=0;
  while wI < gBookmarks.Count do
  begin
    wBD := TStackBookmark(gBookmarks.Items[wI]);
    wNode := BookmarksTree.AddChild(nil, wBD);
    wBD.TreeNode := wNode;
    if gBookmarksItemIndex = wI then
      wSelected := wNode;
    Inc(wI);
  end;
  BookmarksTree.EndUpdate;
  SelectTreeNodeNoEvents(wSelected);
end;

const
  TB_SIZE=240;
  TB_LEFT=140;
  TB_TOP=25;
  TB_HEIGHT=21;

procedure TLazViewStackBookmarks.SetToolBarHorizontal;
begin
  ToolBar.Width:=TB_SIZE;
  ToolBar.Height:=TB_HEIGHT;
  ToolBar.List:=False;
  ToolBar.Left:=TB_LEFT;
  ToolBar.Top:=0;
end;

procedure TLazViewStackBookmarks.SetToolBarVertical;
begin
  ToolBar.Width:=TB_HEIGHT+1;
  ToolBar.Height:=TB_SIZE;
  ToolBar.List:=True;
  ToolBar.Left:=0;
  ToolBar.Top:=TB_TOP;
end;

//Search starts from node  aNode,aIndex
function GetNextStackBookmark(var aIndex: integer; var aNode: PVirtualNode;
  aOnlyCurrentEditorBookmarks: boolean = False): boolean;
var
  wNode: PVirtualNode;
  wI: integer;
  wBD: TStackBookmark;
begin
  Result := False;
  wNode := aNode;
  wI := aIndex + 1;
  aNode := nil;
  aIndex := -1;
  while wI < gBookmarks.Count do
  begin
    wBD := TStackBookmark(gBookmarks.Items[wI]);
    wNode := wBD.TreeNode;
    if (aOnlyCurrentEditorBookmarks = False) or wBD.InCurrentEditor then
    begin
      aNode := wNode;
      aIndex := wI;
      Exit(True);
    end;
    Inc(wI);
  end;
end;

//Search starts from node  aNode,aIndex
function GetPreviousStackBookmark(var aIndex: integer; var aNode: PVirtualNode;
  aOnlyCurrentEditorBookmarks: boolean = False): boolean;
var
  wNode: PVirtualNode;
  wI: integer;
  wBD: TStackBookmark;
begin
  Result := False;
  wNode := aNode;
  wI := aIndex - 1;
  aNode := nil;
  aIndex := -1;
  while wI >= 0 do
  begin
    wBD := TStackBookmark(gBookmarks.Items[wI]);
    wNode:=wBD.TreeNode;
    if (aOnlyCurrentEditorBookmarks = False) or wBD.InCurrentEditor then
    begin
      aNode := wNode;
      aIndex := wI;
      Exit(True);
    end;
    Dec(wI);
  end;
end;

function GetLastStackBookmark(var aIndex: integer; var aNode: PVirtualNode;
  aOnlyCurrentEditorBookmarks: boolean = False): boolean;
var
  wIndex: integer;
  wNode: PVirtualNode;
  wBD: TStackBookmark;
begin
  aIndex := -1;
  aNode := nil;
  wIndex := -1;
  wNode := nil;
  Result := False;
  if gBookmarks.Count > 0 then
  begin
    wIndex := gBookmarks.Count - 1;
    wBD := TStackBookmark(gBookmarks.Items[wIndex]);
    if frmViewStackBM <> nil then
      wNode := frmViewStackBM.BookmarksTree.GetLast();
    if aOnlyCurrentEditorBookmarks and (not wBD.InCurrentEditor) then
    begin
      if not GetPreviousStackBookmark(wIndex, wNode,aOnlyCurrentEditorBookmarks) then
        Exit;
    end;
    Result := True;
  end;
  aIndex := wIndex;
  aNode := wNode;
end;

procedure PushStackBookmark;
var
  wBD: TStackBookmark;
begin
  if gBookmarksCurrentEditor=nil then
    SetActiveEditor;
  if gBookmarksCurrentEditor=nil then
    Exit;
  wBD := TStackBookmark.Create;
  wBD.GetData;
  gBookmarks.Add(wBD);
  Inc(gBookmarksInCurrentEditorCount);
  if frmViewStackBM <> nil then
    wBD.TreeNode:=frmViewStackBM.BookmarksTree.AddChild(nil, wBD);
  SelectBookmark(gBookmarks.Count-1);
end;

procedure InsertStackBookmark;
var
  wIndex: integer;
  wBD: TStackBookmark;
  wNode: PVirtualNode;
begin
  if gBookmarksCurrentEditor=nil then
    SetActiveEditor;
  if gBookmarksCurrentEditor=nil then
    Exit;
  wIndex := gBookmarksItemIndex;
  if wIndex < 0 then
    wIndex := 0;
  wBD := TStackBookmark.Create;
  wBD.GetData;
  gBookmarks.Insert(wIndex, wBD);
  Inc(gBookmarksInCurrentEditorCount);
  if frmViewStackBM <> nil then
  begin
    wNode := frmViewStackBM.BookmarksTree.GetFirstSelected();
    wBD.TreeNode := frmViewStackBM.BookmarksTree.InsertNode(wNode, amInsertBefore, wBD);
  end;
  SelectBookmark(wIndex);
end;

procedure FirstStackBookmark(aOnlyCurrentEditorBookmarks: boolean = False);
var
  wIndex: integer;
  wNode: PVirtualNode;
  wBD: TStackBookmark;
begin
  if gBookmarks.Count > 0 then
  begin
    wIndex := 0;
    wBD := TStackBookmark(gBookmarks.Items[wIndex]);
    wNode := wBD.TreeNode;
    if aOnlyCurrentEditorBookmarks and (not wBD.InCurrentEditor) then
    begin
      if not GetNextStackBookmark(wIndex, wNode, aOnlyCurrentEditorBookmarks) then
        Exit;
    end;
    SelectBookmark(wIndex);
    GoToCurrentBookmark;
  end;
end;

procedure LastStackBookmark(aOnlyCurrentEditorBookmarks: boolean = False);
var
  wIndex: integer;
  wNode: PVirtualNode;
begin
  {$PUSH}
  {$WARN 5057 off}
  if GetLastStackBookmark(wIndex, wNode, aOnlyCurrentEditorBookmarks) then
  begin
    SelectBookmark(wIndex);
    GoToCurrentBookmark;
  end;
  {$POP}
end;

function NextStackBookmark(aOnlyCurrentEditorBookmarks: boolean = False): boolean;
var
  wNode: PVirtualNode;
  wIndex: integer;
begin
  Result := False;
  wNode := nil;
  wIndex := gBookmarksItemIndex;
  if frmViewStackBM <> nil then
    wNode := frmViewStackBM.BookmarksTree.GetFirstSelected;
  if GetNextStackBookmark(wIndex, wNode, aOnlyCurrentEditorBookmarks) then
  begin
    SelectBookmark(wIndex);
    GoToCurrentBookmark;
    Result := True;
  end;
end;

function PreviousStackBookmark(aOnlyCurrentEditorBookmarks: boolean = False): boolean;
var
  wNode: PVirtualNode;
  wIndex: integer;
begin
  Result := False;
  wNode := nil;
  wIndex := gBookmarksItemIndex;
  if frmViewStackBM <> nil then
    wNode := frmViewStackBM.BookmarksTree.GetFirstSelected;
  if GetPreviousStackBookmark(wIndex, wNode, aOnlyCurrentEditorBookmarks) then
  begin
    SelectBookmark(wIndex);
    GoToCurrentBookmark;
    Result := True;
  end;
end;

procedure PopStackBookmark(aOnlyCurrentEditorBookmarks: boolean = False);
var
  wIndex: integer;
  wBD: TStackBookmark;
  wNode: PVirtualNode;
begin
  {$PUSH}
  {$WARN 5057 off}
  if GetLastStackBookmark(wIndex, wNode, aOnlyCurrentEditorBookmarks) then
  begin
    wBD := TStackBookmark(gBookmarks.Items[wIndex]);
    GoToBookmark(wBD);
    if wBD.Locked then
      Exit;
    if wBD.InCurrentEditor then
      Dec(gBookmarksInCurrentEditorCount);
    gBookmarks.Delete(wIndex);
    DeleteTreeNodeNoEventsStatic(wNode, aOnlyCurrentEditorBookmarks);
    // keep selected item.
    if wIndex<gBookmarksItemIndex then
      Dec(gBookmarksItemIndex);
    SelectBookmark(gBookmarksItemIndex);
  end;
  {$POP}
end;

procedure SwapStackBookmark(aOnlyCurrentEditorBookmarks: boolean = False);
var
  wIndex: integer;
  wBD: TStackBookmark;
  wOldBD: TStackBookmark;
  wNode: PVirtualNode;
begin
  {$PUSH}
  {$WARN 5057 off}
  if gBookmarksCurrentEditor=nil then
    Exit;
  wOldBD := nil;
  //wIndex := gBookmarks.Count - 1;
  GetLastStackBookmark(wIndex, wNode, aOnlyCurrentEditorBookmarks);
  if wIndex >= 0 then
  begin
    try
      wBD := TStackBookmark(gBookmarks.Items[wIndex]);
      if wBD.InCurrentEditor then
        Dec(gBookmarksInCurrentEditorCount);
      wOldBD := wBD.Clone;
      wBD.GetData;
      if wOldBD.Edited then //keep description if edited.
      begin
        wBD.Edited := True;
        wBD.Description := wOldBD.Description;
      end;
      if wBD.InCurrentEditor then
        Inc(gBookmarksInCurrentEditorCount);
      GotoBookmark(wOldBD);
      if frmViewStackBM <> nil then
        frmViewStackBM.BookmarksTree.Invalidate;
      SelectBookmark(wIndex);
    finally
      wOldBD.Free;
    end;
  end;
  {$POP}
end;

procedure TLazViewStackBookmarks.btnPushClick(Sender: TObject);
begin
  PushStackBookmark;
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.btnInsertClick(Sender: TObject);
begin
  InsertStackBookmark;
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.btnLastClick(Sender: TObject);
begin
  LastStackBookmark(SHIFTKEY_CODE in GetKeyShiftState);
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.btnFirstClick(Sender: TObject);
begin
  FirstStackBookmark(SHIFTKEY_CODE in GetKeyShiftState);
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.FormResize(Sender:TObject);
begin
  if Height>=(ToolBar.Top+TB_SIZE) then
  begin
    if ToolBar.Width>ToolBar.Height then
    begin
      SetToolbarVertical;
      BookmarksTree.Left:=24;
      BookmarksTree.Width:=Width-BookmarksTree.Left-3;
    end;
  end
  else
  begin
    if (ToolBar.Width<ToolBar.Height) and (Width>(TB_LEFT+TB_SIZE))  then
    begin
      SetToolbarHorizontal;
      BookmarksTree.Left:=2;
      BookmarksTree.Width:=Width-BookmarksTree.Left-3;
    end;
  end;
end;

procedure TLazViewStackBookmarks.btnPreviousClick(Sender: TObject);
begin
  PreviousStackBookmark(SHIFTKEY_CODE in GetKeyShiftState);
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.btnNextClick(Sender: TObject);
begin
  NextStackBookmark(SHIFTKEY_CODE in GetKeyShiftState);
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.btnPopClick(Sender: TObject);
begin
  PopStackBookmark(SHIFTKEY_CODE in GetKeyShiftState);
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.btnSwapClick(Sender: TObject);
begin
  SwapStackBookmark(SHIFTKEY_CODE in GetKeyShiftState);
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.btnClearClick(Sender: TObject);
begin
  if SHIFTKEY_CODE in GetKeyShiftState then
    ClearBookmarksListOnlyCurrentEditor
  else
    ClearBookmarksList;
end;

//Refresh descriptions of all the bookmarks in the current editor.
//Better than catch all editor changes.
//only scans the bookmarks of the current editor
procedure TLazViewStackBookmarks.btnRefreshClick(Sender: TObject);
var
  wI: integer;
  wBD: TStackBookmark;
begin
  wI:=0;
  while wI < gBookmarks.Count do
  begin
    wBD := TStackBookmark(gBookmarks.Items[wI]);
    if wBD.InCurrentEditor and (not wBD.Edited) then  //keep user edited description.
      wBD.GetDescription;
    Inc(wI);
  end;
  BookmarksTree.Invalidate;
end;

procedure TLazViewStackBookmarks.btnDeleteClick(Sender: TObject);
var
  wIndex: integer;
  wNode: PVirtualNode;
  wBD: TStackBookmark;
begin
  wNode := BookmarksTree.GetFirstSelected;
  if wNode <> nil then
  begin
    wIndex := wNode^.Index;
    wBD := TStackBookmark(gBookmarks.Items[wIndex]);
    if wBD.InCurrentEditor then
      Dec(gBookmarksInCurrentEditorCount);
    gBookmarks.Delete(wIndex);
    DeleteTreeNodeNoEvents(wNode, True);
    SelectBookmark(wIndex);
  end;
  ReturnFocusToEditor;
end;

procedure TLazViewStackBookmarks.btnEditClick(Sender: TObject);
begin
  if gBookmarksItemIndex >= 0 then
    BookmarksTree.EditNode(BookmarksTree.GetFirstSelected(), -1);
end;

procedure TLazViewStackBookmarks.btnLockClick(Sender: TObject);
var
  wBD: TStackBookmark;
begin
  if (gBookmarksItemIndex >= 0) and (gBookmarksItemIndex<gBookmarks.Count) then
  begin
    wBD := TStackBookmark(gBookmarks.Items[gBookmarksItemIndex]);
    wBD.Locked := not wBD.Locked;
    BookmarksTree.Invalidate;
  end;
end;

procedure TLazViewStackBookmarks.FormCreate(Sender: TObject);
begin
  BookmarksTree.NodeDataSize := SizeOf(Pointer);
  BookmarksTree.DefaultNodeHeight := -GetActualFont(BookmarksTree).Height * 2 + 10;
  //BookmarksTree.DefaultNodeHeight:=34;

  btnPush.Caption := rsBookmarkPush;
  btnPop.Caption := rsBookmarkPop;
  btnSwap.Caption := rsBookmarkSwap;

  //Hints
  btnPush.Hint := rsBtnPushHint;
  btnPush.ShowHint := True;
  btnPop.Hint := rsBtnPopHint;
  btnPop.ShowHint := True;
  btnSwap.Hint := rsBtnSwapHint;
  btnSwap.ShowHint := True;
  btnLock.Hint := rsBtnLockHint;
  btnLock.ShowHint := True;
  // toolbar
  tbbFirst.Hint := rsBtnFirstHint;
  tbbFirst.ShowHint := True;
  tbbPrevious.Hint := rsBtnPreviousHint;
  tbbPrevious.ShowHint := True;
  tbbNext.Hint := rsBtnNextHint;
  tbbNext.ShowHint := True;
  tbbLast.Hint := rsBtnLastHint;
  tbbLast.ShowHint := True;
  tbbInsert.Hint := rsBtnInsertHint;
  tbbInsert.ShowHint := True;
  tbbDelete.Hint := rsBtnDeleteHint;
  tbbDelete.ShowHint := True;
  tbbEdit.Hint := rsBtnEditHint;
  tbbEdit.ShowHint := True;
  tbbClear.Hint := rsBtnClearHint;
  tbbClear.ShowHint := True;
  tbbRefresh.Hint := rsBtnRefreshHint;
  tbbRefresh.ShowHint := True;
  tbbExport.Hint := rsBtnExportHint;
  tbbExport.ShowHint := True;
  tbbImport.Hint := rsBtnImportHint;
  tbbImport.ShowHint := True;

  FillTreeView;
  SetActiveEditor;
end;

procedure TLazViewStackBookmarks.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if gBookmarksCurrentEditor <> nil then
    ClearBookmarksNodePointers;
  BookmarksTree.Clear;
  CloseAction := caFree;
  frmViewStackBM := nil;
end;

procedure TLazViewStackBookmarks.BookmarksTreeDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const CellText: string;
  const CellRect: TRect; var DefaultDraw: boolean);
var
  wBD: TStackBookmark;
  wBrushColor: TColor;
begin
  wBD := TStackBookmark(Sender.GetNodeData(Node)^);
  TargetCanvas.TextOut(10, 0, '[' + IntToStr(wBD.XY.Y) + ',' + IntToStr(wBD.XY.X) + ']   ' + wBD.Description);
  if Length(wBD.UnitnameSB)> 0 then
  begin
    TargetCanvas.Font.Bold:=wBD.InCurrentEditor;
    TargetCanvas.TextOut(10, 16,{IntToStr(Node^.Index)+'  '+}wBD.UnitnameSB);
    TargetCanvas.Font.Bold:=false;
  end;
  if wBD.Locked then
  begin
    wBrushColor := TargetCanvas.Brush.Color;
    TargetCanvas.Brush.Color := clMaroon;
    TargetCanvas.FillRect(2, 4, 7, 9);
    TargetCanvas.Brush.Color := wBrushColor;
  end;
  DefaultDraw := False;
end;

{
procedure TLazViewStackBookmarks.BookmarksTreeBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
begin
  // ZEBRA STYLE
  if (Node^.Index and 1) <> 0 then
  begin
    ItemColor := ALTERNATE_ITEM_COLOR;
    EraseAction := eaColor;
  end;
end;
}

procedure TLazViewStackBookmarks.BookmarksTreeAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if (not FIgnoreEvents) and (Node <> nil) then
    gBookmarksItemIndex := Node^.Index;
end;

procedure TLazViewStackBookmarks.BookmarksTreeKeyAction(Sender: TBaseVirtualTree;
  var CharCode: word; var Shift: TShiftState; var DoDefault: boolean);
var
  wNode: PVirtualNode;
begin
  if CharCode = $0D then    //Enter key
  begin
    wNode := BookmarksTree.GetFirstSelected();
    if wNode <> nil then
      GoToBookmark(wNode^.Index);
  end;
end;

procedure TLazViewStackBookmarks.BookmarksTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: string);
var
  wBD: TStackBookmark;
begin
  wBD := TStackBookmark(Sender.GetNodeData(Node)^);
  wBD.Description := NewText;
  wBD.Edited:=True;
end;

procedure TLazViewStackBookmarks.BookmarksTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  wBD: TStackBookmark;
begin
  wBD := TStackBookmark(Sender.GetNodeData(Node)^);
  if length(wBD.Description)>0 then
    CellText := wBD.Description
  else
    CellText := ' ';
end;

procedure TLazViewStackBookmarks.BookmarksTreeDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: boolean);
begin
  Allowed := True;
end;

procedure TLazViewStackBookmarks.BookmarksTreeDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  const Pt: TPoint; var Effect: longword; Mode: TDropMode);
var
  wSource, wTarget: PVirtualNode;
  wAttMode: TVTNodeAttachMode;
  wBDSource: PStackBookmark;
  wIndexSource, wIndexTarget, wItemIndex: integer;
begin
  wSource := TLazVirtualStringTree(Source).FocusedNode;
  wTarget := Sender.DropTargetNode;
  wIndexSource := wSource^.Index;
  if wTarget <> nil then
    wIndexTarget := wTarget^.Index
  else
    wIndexTarget := gBookmarks.Count - 1;  // insert after last item
  wItemIndex := wIndexTarget;
  case Mode of
    dmNowhere:
    begin
      wAttMode := amInsertAfter; //amNoWhere;
      wTarget := TLazVirtualStringTree(Source).GetLast();
      Inc(wIndexTarget);
    end;
    dmAbove: wAttMode := amInsertBefore;
    dmOnNode, dmBelow:
    begin
      wAttMode := amInsertAfter;
      Inc(wIndexTarget);
    end;
  end;
  if wSource = wTarget then
    Exit;
  //ShowMessage(IntToStr(wIndexSource)+'->'+IntToStr(wIndexTarget));
  if wIndexSource <> wIndexTarget then
  begin
    wBDSource := PStackBookmark(gBookmarks.Items[wIndexSource]);
    gBookmarks.Insert(wIndexTarget, wBDSource);
    if wIndexTarget < wIndexSource then  //we inserted before.
      Inc(wIndexSource);
    gBookmarks.Delete(wIndexSource);
    gBookmarksItemIndex := wItemIndex;
    Sender.MoveTo(wSource, wTarget, wAttMode, False);
  end;
end;

procedure TLazViewStackBookmarks.BookmarksTreeDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
  var Effect: longword; var Accept: boolean);
begin
  Accept := (Source = Sender);
end;

procedure TLazViewStackBookmarks.BookmarksTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  wBD: TStackBookmark;
begin
  wBD := TStackBookmark(Sender.GetNodeData(Node)^);
  LineBreakStyle := hlbForceMultiLine;
  HintText := ' ' + HintText + wBD.Description;
  HintText := HintText + #10 + #10 + ' ';
  if wBD.FilenameIsRelative then
    HintText := HintText + '*' + gBookMarksProjectFolder;
  HintText := HintText + wBD.Filename;
end;

procedure TLazViewStackBookmarks.BookmarksTreeNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  if HitInfo.HitNode <> nil then
  begin
    gBookmarksItemIndex := HitInfo.HitNode^.Index;
    GoToBookmark(HitInfo.HitNode^.Index);
  end;
end;


{// sample configuration file.
 // directory separator /  avoids problems with escape chars \n etc.
<?xml version="1.0" encoding="UTF-8"?>
<StackBookmarks Version="1" ProjectPath="C:/Sources/StackBookmarks/">
  <Bookmarks Count="3" Current="1">
    <Bookmark Edited="0" Locked="0" X="8" Y="37" TopLine="22" IsRelative="1">
      <Description>  TlazIdeMiniMap = class(TForm)</Description>
      <Filename>C:/StackBookmarks/unit1.pas</Filename>
      <Unitname>unit1</Unitname>
    </Bookmark>
    <Bookmark Edited="1" Locked="1" X="4" Y="78" TopLine="58" IsRelative="1">
      <Description>implementation EDITED</Description>
      <Filename>C:/StackBookmarks/unit1.pas</Filename>
      <Unitname>unit1</Unitname>
    </Bookmark>
    <Bookmark Edited="0" Locked="0" X="9" Y="113" TopLine="88" IsRelative="1">
      <Description>procedure Register;</Description>
      <Filename>C:/StackBookmarks/unit1.pas</Filename>
      <Unitname>unit1</Unitname>
    </Bookmark>
  </Bookmarks>
</StackBookmarks>
}

procedure SaveStackBookmarks(const aFilename: string);
var
  wDoc: TXMLDocument;
  wRootNode, wBookmarks, wBookmark, wField, wValue: TDOMNode;
  wI: integer;
  wBD: TStackBookmark;
begin
  try
    wDoc := TXMLDocument.Create;
    wRootNode := wDoc.CreateElement('StackBookmarks');
    TDOMElement(wRootNode).SetAttribute('Version', '1');
    TDOMElement(wRootNode).SetAttribute('ProjectPath', SwitchPathDelims(gBookMarksProjectFolder,pdsUnix));
    wDoc.Appendchild(wRootNode);

    wBookmarks := wDoc.CreateElement('Bookmarks');
    TDOMElement(wBookmarks).SetAttribute('Count', IntToStr(gBookmarks.Count));
    TDOMElement(wBookmarks).SetAttribute('Current', IntToStr(gBookmarksItemIndex));
    wRootNode.AppendChild(wBookmarks);
    wI:=0;
    while wI < gBookmarks.Count do
    begin
      wBD := TStackBookmark(gBookmarks.Items[wI]);
      wBookmark := wDoc.CreateElement('Bookmark');
      TDOMElement(wBookmark).SetAttribute('Edited', IntToStr(integer(wBD.Edited)));
      TDOMElement(wBookmark).SetAttribute('Locked', IntToStr(integer(wBD.Locked)));
      TDOMElement(wBookmark).SetAttribute('X', IntToStr(wBD.XY.X));
      TDOMElement(wBookmark).SetAttribute('Y', IntToStr(wBD.XY.Y));
      TDOMElement(wBookmark).SetAttribute('TopLine', IntToStr(wBD.TopLine));
      TDOMElement(wBookmark).SetAttribute('IsRelative', IntToStr(integer(wBD.FilenameIsRelative)));
      wField := wDoc.CreateElement('Description');
      wValue := wDoc.CreateTextNode(wBD.Description);
      wField.Appendchild(wValue);
      wBookmark.Appendchild(wField);
      wField := wDoc.CreateElement('Filename');
      wValue := wDoc.CreateTextNode(wBD.Filename);
      wField.Appendchild(wValue);
      wBookmark.Appendchild(wField);
      wField := wDoc.CreateElement('Unitname');
      wValue := wDoc.CreateTextNode(wBD.UnitnameSB);
      wField.Appendchild(wValue);
      wBookmark.Appendchild(wField);

      wBookmarks.AppendChild(wBookmark);
      Inc(wI);
    end;
    writeXMLFile(wDoc, aFilename);
  finally
    wDoc.Free;
  end;
end;

function GetBooleanAtributte(aDOMElement: TDOMElement; const aName: string): boolean;
var
  wStr: string;
begin
  wStr := aDOMElement.getAttribute(aName);
  Result := (length(wStr) > 0) and (wStr <> '0');
end;

procedure LoadStackBookmarks(const aFilename: string;aImport:boolean=false);
var
  wStackBookmarks,wBookmarks, wBookmark: TDOMElement;
  wDoc: TXMLDocument;
  wKey: TDOMNode;
  wCount, wCurrent: integer;
  wI: integer;
  wBD: TStackBookmark;
  wImportedProjectPath:string;
  wFilename:string;
begin
  if not FileExists(aFilename) then
    Exit;
  wDoc := nil;
  try
    ReadXMLFile(wDoc, aFilename);
    wStackBookmarks := TDOMElement(wDoc.DocumentElement);  //root node
    if wStackBookmarks = nil then
      Exit;
    wImportedProjectPath:=SwitchPathDelims(wStackBookmarks.getAttribute('ProjectPath'),pdsSystem);

    wBookmarks := TDOMElement(wDoc.DocumentElement.FindNode('Bookmarks'));
    if wBookmarks = nil then
      Exit;
    wCount := StrToIntDef(wBookmarks.getAttribute('Count'), 0);
    wCurrent := STrToIntDef(wBookmarks.getAttribute('Current'), -1);

    wBookmark := TDOMElement(wBookmarks.FirstChild);
    wI := 0;
    while wI < wCount do
    begin
      if wBookmark = nil then
        break;
      wBD := TStackBookmark.Create;
      wBD.Edited := GetBooleanAtributte(wBookmark,'Edited');
      wBD.Locked := GetBooleanAtributte(wBookmark,'Locked');
      wBD.FilenameIsRelative:=GetBooleanAtributte(wBookmark,'IsRelative');
      wBD.XY.X := StrToIntDef(wBookmark.getAttribute('X'), 0);
      wBD.XY.Y := StrToIntDef(wBookmark.getAttribute('Y'), 0);
      wBD.TopLine := StrToIntDef(wBookmark.getAttribute('TopLine'), 0);
      wKey := wBookmark.FindNode('Description');
      if (wKey <> nil) and (wKey.FirstChild<>nil) then
        wBD.Description := wKey.FirstChild.NodeValue;
      wKey := wBookmark.FindNode('Unitname');
      if (wKey <> nil) and (wKey.FirstChild<>nil) then
        wBD.UnitnameSB := wKey.FirstChild.NodeValue;
      wKey := wBookmark.FindNode('Filename');
      if (wKey <> nil) and (wKey.FirstChild<>nil) then
        wBD.Filename := wKey.FirstChild.NodeValue;

      if aImport and wBD.FilenameIsRelative then
      begin
        if not FileExists(gBookMarksProjectFolder+wBD.Filename) then
        begin
          //make filename absolute
          wFileName:=wImportedProjectPath+wBD.Filename;
          //rebase with actual projectpath
          wBD.FileName:=GetNormalizedFileName(gBookMarksProjectFolder,wFileName,wBD.FilenameIsRelative);
        end;
      end;
      wBD.FilenameHash := HashString(wBD.Filename);
      if wBD.InCurrentEditor then
        Inc(gBookmarksInCurrentEditorCount);
      gBookmarks.Add(wBD);
      wBookmark := TDOMElement(wBookmark.NextSibling);
      Inc(wI);
    end;
    gBookmarksItemIndex := wCurrent;
  finally
    wDoc.Free;
  end;
end;

procedure TLazViewStackBookmarks.btnExportClick(Sender: TObject);
var
  wSaveDlg: TSaveDialog;
begin
  if gBookmarks.Count <= 0 then
    Exit;
  wSaveDlg := TSaveDialog.Create(nil);
  try
    wSaveDlg.DefaultExt := '.bkm';
    wSaveDlg.Filter := rsImpExpFilter;
    wSaveDlg.FilterIndex := 0;
    wSaveDlg.Title := rsExportTitle;
    if wSaveDlg.Execute = True then
      SaveStackBookmarks(wSaveDlg.FileName);
  finally
    wSaveDlg.Free;
  end;
end;

procedure TLazViewStackBookmarks.btnImportClick(Sender:TObject);
var
  wOpenDlg: TOpenDialog;
  wCurrentIndex:integer;
begin
  wOpenDlg := TOpenDialog.Create(nil);
  try
    wOpenDlg.DefaultExt := '.bkm';
    wOpenDlg.Filter := rsImpExpFilter;
    wOpenDlg.FilterIndex := 0;
    wOpenDlg.Title := rsImportTitle;
    if wOpenDlg.Execute = True then
    begin
      wCurrentIndex:=gBookmarksItemIndex;
      if gBookmarks.Count<=0 then
        wCurrentIndex:=-1;
      LoadStackBookmarks(wOpenDlg.FileName,True);
      if wCurrentIndex<>-1 then
        gBookmarksItemIndex := wCurrentIndex;
      if frmViewStackBM <> nil then
      begin
        frmViewStackBM.BookmarksTree.Clear;
        frmViewStackBM.FillTreeView;
      end;
    end;
  finally
    wOpenDlg.Free;
  end;
end;

type
  //need access to protected function.
  //I'm too lazy to derive a new class
  TSynEditHack = class(TSynEdit)
  end;

procedure TCaptureIdeEvents.EditorAddChangeHandler;
var
  wSV: TSynEditStringsLinked;
begin
  if gBookmarksCurrentEditor = nil then
    Exit;
  if not FEnabled then
    Exit;
  if gBookmarksCurrentEditor = nil then
    Exit;
  try
    wSV := TSynEditHack(gBookmarksCurrentEditor).GetViewedTextBuffer;
    //if gBookmarksCurrentEditor.TextViewsManager=nil then
    //  Exit;
    //if gBookmarksCurrentEditor.TextViewsManager.Count<=0 then
    //  Exit;
    //wSV:=gBookmarksCurrentEditor.TextViewsManager.SynTextView[0];
    if wSV <> nil then
      wSV.AddChangeHandler(senrLineCount, @OnLineCountChanged);
  except

  end;
end;

procedure TCaptureIdeEvents.EditorRemoveChangeHandler;
var
  wSV: TSynEditStringsLinked;
begin
  if gBookmarksCurrentEditor = nil then
    Exit;
  try
    wSV := TSynEditHack(gBookmarksCurrentEditor).GetViewedTextBuffer;
    //if gBookmarksCurrentEditor.TextViewsManager=nil then
    //  Exit;
    //if gBookmarksCurrentEditor.TextViewsManager.Count<=0 then
    //  Exit;
    //wSV:=gBookmarksCurrentEditor.TextViewsManager.SynTextView[0];
    if wSV <> nil then
      wSV.RemoveChangeHandler(senrLineCount, @OnLineCountChanged);
  except

  end;
end;

procedure TCaptureIdeEvents.OnActiveEditorChanged(Sender:TObject);
begin
  //ShowMessage('changed editor');
  if gCaptureIdeEventsObject=nil then
    Exit;
  if FEnabled then
    SetActiveEditor;
end;

function TCaptureIdeEvents.OnProjectOpened(Sender:TObject; AProject:TLazProject):TModalResult;
begin
  //ShowMessage('project opened');
  if gCaptureIdeEventsObject=nil then
    Exit;
  FEnabled:=True;
  gBookMarksProjectFolder:=ExtractFilePath(AProject.ProjectSessionFile);
  SetActiveEditor;
  ClearBookmarksList;
  LoadStackBookmarks(ChangeFileExt(AProject.ProjectSessionFile,'.bkm'));
  if frmViewStackBM<>nil then
    frmViewStackBM.FillTreeView;
  gBookmarksInCurrentEditorCount:=CountBookmarksInCurrentEditor;
  result:=mrOk;
end;

function TCaptureIdeEvents.OnFileSaved(Sender:TObject; aFile:TLazProjectFile;
  SaveStep:TSaveEditorFileStep; TargetFilename:string):TModalResult;
var
  wOldHash:UInt32;
  wFileName:string;
  wUnitName:string;
  wI:integer;
  wBD:TStackBookmark;
  wIsRelative:boolean;
begin
  if gCaptureIdeEventsObject=nil then
    Exit;
  if SaveStep=sefsSavedAs then
  begin
    wOldHash:=gBookmarksCurrentEditorFilenameHash;
    wFileName:='';
    wUnitName:='';
    if GetActiveEditorData(wFileName,wUnitName,wIsRelative) then
    begin
      gBookmarksCurrentEditorFilename := wFileName;
      gBookmarksCurrentEditorFilenameIsRelative:=wIsRelative;
      gBookmarksCurrentEditorUnitname := wUnitName;
      gBookmarksCurrentEditorFilenameHash := HashString(gBookmarksCurrentEditorFilename);
      wI:=0;
      while wI<gBookmarks.Count do
      begin
        //replace  with new filename
        wBD := TStackBookmark(gBookmarks.Items[wI]);
        if wBD.FilenameHash=wOldHash then
        begin
          wBD.Filename:=gBookmarksCurrentEditorFilename;
          wBD.UnitnameSB:=gBookmarksCurrentEditorUnitname;
          wBD.FilenameHash:=gBookmarksCurrentEditorFilenameHash;
        end;
        Inc(wI);
      end;
    end;
    if frmViewStackBM<>nil then
      frmViewStackBM.BookmarksTree.Invalidate;
    //ShowMessage('Saved as old: '+TargetFileName);
  end;
  result:=mrOk;
end;

function TCaptureIdeEvents.OnProjectClose(Sender:TObject; AProject:TLazProject):TModalResult;
begin
  //ShowMessage('project closed');
  if gCaptureIdeEventsObject=nil then
    Exit;
  FEnabled:=False;
  if gCaptureIdeEventsObject=nil then
    Exit;

  gCaptureIdeEventsObject.EditorRemoveChangeHandler;
  gBookmarksCurrentEditor:=nil;

  SaveStackBookmarks(ChangeFileExt(AProject.ProjectSessionFile,'.bkm'));
  ClearBookmarksList;
  result:=mrOk;
end;

// destroy all/ suicide.
procedure TCaptureIdeEvents.OnIdeClose(Sender: TObject);
begin
  if gCaptureIdeEventsObject=nil then
    Exit;
  //LazarusIDE.RemoveHandlerOnProjectOpened(@gCaptureIdeEventsObject.OnProjectOpened);
  //LazarusIDE.AddHandlerOnProjectClose(@gCaptureIdeEventsObject.OnProjectClose);
  //LazarusIDE.AddHandlerOnSaveEditorFile(@gCaptureIdeEventsObject.OnFileSaved);
  SourceEditorManagerIntf.UnRegisterChangeEvent(semEditorActivate,@gCaptureIdeEventsObject.OnActiveEditorChanged);
  ClearBookmarksList;
  FreeAndNil(gBookmarks);
  FreeAndNil(gCaptureIdeEventsObject);   // auto destruction.
end;

procedure TCaptureIdeEvents.DoPush(Sender:TObject);
begin
  PushStackBookmark;
end;

procedure TCaptureIdeEvents.DoPop(Sender:TObject);
begin
  PopStackBookmark(false);
end;

procedure TCaptureIdeEvents.DoSwap(Sender:TObject);
begin
  SwapStackBookmark(false);
end;

procedure TCaptureIdeEvents.DoFirst(Sender:TObject);
begin
  FirstStackBookmark(false);
end;

procedure TCaptureIdeEvents.DoPrevious(Sender:TObject);
begin
  PreviousStackBookmark(false);
end;

procedure TCaptureIdeEvents.DoNext(Sender:TObject);
begin
  NextStackBookmark(false);
end;

procedure TCaptureIdeEvents.DoLast(Sender:TObject);
begin
  LastStackBookmark(false);
end;

procedure TCaptureIdeEvents.DoPopCurrent(Sender:TObject);
begin
  PopStackBookmark(true);
end;

procedure TCaptureIdeEvents.DoSwapCurrent(Sender:TObject);
begin
  SwapStackBookmark(true);
end;

procedure TCaptureIdeEvents.DoFirstCurrent(Sender:TObject);
begin
  FirstStackBookmark(true);
end;

procedure TCaptureIdeEvents.DoPreviousCurrent(Sender:TObject);
begin
  PreviousStackBookmark(true);
end;

procedure TCaptureIdeEvents.DoNextCurrent(Sender:TObject);
begin
  NextStackBookmark(true);
end;

procedure TCaptureIdeEvents.DoLastCurrent(Sender:TObject);
begin
  LastStackBookmark(true);
end;

procedure ShowlazStackBookmarks(Sender:TObject);
begin
  IDEWindowCreators.ShowForm(lazStackBookmarksCreator.FormName, True);
end;

procedure CreatelazStackBookmarks(Sender: TObject; aFormName: string; var aForm: TCustomForm;
  DoDisableAutoSizing: boolean);
begin
  aForm:=nil;
  if frmViewStackBM<>nil then
    Exit;
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName, FORM_NAME) <> 0 then
  begin
    DebugLn(['ERROR: CreatelazStackBookmark: there is already a form with this ' + 'name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(aForm, TLazViewStackBookmarks, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  if aForm<>nil then
  begin
    aForm.Name := aFormName;
    frmViewStackBM := aForm as TLazViewStackBookmarks;
  end;
end;

const
  SB_MENU_NAME = 'sbStackBookmarks';
  SB_CATEGORY_IDECMD_NAME = 'sbStackBookmarks';
  SB_PUSH      = 'sbPush';
  SB_POP       = 'sbPop';
  SB_SWAP      = 'sbSwap';
  SB_FIRST     = 'sbFirst';
  SB_PREVIOUS   = 'sbPrevious';
  SB_NEXT      = 'sbNext';
  SB_LAST      = 'sbLast';

  SB_C_POP       = 'sbcPop';
  SB_C_SWAP      = 'sbcSwap';
  SB_C_FIRST     = 'sbcFirst';
  SB_C_PREVIOUS   = 'sbcPrevious';
  SB_C_NEXT      = 'sbcNext';
  SB_C_LAST      = 'sbcLast';

procedure Register;
var
  Cat: TIDECommandCategory;
  Cmd: TIDECommand;
  MainMenu, SourceEditorPopupMenu, CatCursor,CatCursorPopup: TIDEMenuSection;
  Key: TIDEShortCut;

  procedure RegisterSBMenuItem(const aName:string;const aMenuCaption:string;const aCmdDescription:string;const aOnClick: TNotifyEvent = nil);
  begin
    Key := IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []);
    Cmd := RegisterIDECommand(Cat, aName, aCmdDescription, Key, aOnClick);
    RegisterIDEMenuCommand(CatCursor, aName, aMenuCaption, aOnClick, nil, Cmd);
    RegisterIDEMenuCommand(CatCursorPopup, aName, aMenuCaption, aOnClick, nil, Cmd);
  end;

begin
  // register shortcut and menu item
  Cat := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  Cmd := RegisterIDECommand(Cat, 'ViewlazStackBookmarks',
    rsSBMenuCaption, IDEShortCut(VK_UNKNOWN, []),
    CleanIDEShortCut, nil, @ShowlazStackBookmarks);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows, 'ViewlazStackBookmarks',
    rsSBMenuCaption, nil, nil, Cmd);
  // Register Item in Search and subitems.
  Cat := IDECommandList.CreateCategory(nil, SB_CATEGORY_IDECMD_NAME,
    rsSB_CATEGORY_IDECMD, IDECmdScopeSrcEditOnly);
  MainMenu := RegisterIDESubMenu(itmBookmarks, SB_MENU_NAME, rsSB_MENU);
  SourceEditorPopupMenu := RegisterIDESubMenu(SrcEditMenuSectionMarks, SB_MENU_NAME, rsSB_MENU);
  // register all items in main toolbar menus and in source editor popup menu.

  Key := IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []);
  Cmd := RegisterIDECommand(Cat, SB_PUSH, rsSB_PUSH_IDECMD,
    Key, @gCaptureIdeEventsObject.DoPush);
  RegisterIDEMenuCommand(MainMenu, SB_PUSH, rsSB_PUSH_MENU,
    @gCaptureIdeEventsObject.DoPush, nil, Cmd);
  RegisterIDEMenuCommand(SourceEditorPopupMenu, SB_PUSH, rsSB_PUSH_MENU,
    @gCaptureIdeEventsObject.DoPush, nil, Cmd);

  //ALL BOOKMARKS
  CatCursor := RegisterIDEMenuSection(MainMenu,'sbCursor');
  CatCursorPopup := RegisterIDEMenuSection(SourceEditorPopupMenu,'sbCursor');

  RegisterSBMenuItem(SB_POP,rsSB_POP_MENU,rsSB_POP_IDECMD,@gCaptureIdeEventsObject.DoPop);
  RegisterSBMenuItem(SB_SWAP,rsSB_SWAP_MENU,rsSB_SWAP_IDECMD,@gCaptureIdeEventsObject.DoSwap);
  RegisterSBMenuItem(SB_FIRST,rsSB_FIRST_MENU,rsSB_FIRST_IDECMD,@gCaptureIdeEventsObject.DoFirst);
  RegisterSBMenuItem(SB_PREVIOUS,rsSB_PREVIOUS_MENU,rsSB_PREVIOUS_IDECMD,@gCaptureIdeEventsObject.DoPrevious);
  RegisterSBMenuItem(SB_NEXT,rsSB_NEXT_MENU,rsSB_NEXT_IDECMD,@gCaptureIdeEventsObject.DoNext);
  RegisterSBMenuItem(SB_LAST,rsSB_LAST_MENU,rsSB_LAST_IDECMD,@gCaptureIdeEventsObject.DoLast);

  // ONLY BOOKMARKS IN CURRENT EDITOR
  CatCursor := RegisterIDEMenuSection(MainMenu,'sbcCursor');
  CatCursorPopup := RegisterIDEMenuSection(SourceEditorPopupMenu,'sbcCursor');

  RegisterSBMenuItem(SB_C_POP,rsSB_C_POP_MENU,rsSB_C_POP_IDECMD,@gCaptureIdeEventsObject.DoPopCurrent);
  RegisterSBMenuItem(SB_C_SWAP,rsSB_C_SWAP_MENU,rsSB_C_SWAP_IDECMD,@gCaptureIdeEventsObject.DoSwapCurrent);
  RegisterSBMenuItem(SB_C_FIRST,rsSB_C_FIRST_MENU,rsSB_C_FIRST_IDECMD,@gCaptureIdeEventsObject.DoFirstCurrent);
  RegisterSBMenuItem(SB_C_PREVIOUS,rsSB_C_PREVIOUS_MENU,rsSB_C_PREVIOUS_IDECMD,@gCaptureIdeEventsObject.DoPreviousCurrent);
  RegisterSBMenuItem(SB_C_NEXT,rsSB_C_NEXT_MENU,rsSB_C_NEXT_IDECMD,@gCaptureIdeEventsObject.DoNextCurrent);
  RegisterSBMenuItem(SB_C_LAST,rsSB_C_LAST_MENU,rsSB_C_LAST_IDECMD,@gCaptureIdeEventsObject.DoLastCurrent);

  // register dockable Window
  lazStackBookmarksCreator := IDEWindowCreators.Add(FORM_NAME, @CreatelazStackBookmarks,
    nil, '100', '100', '366', '512'  // default place at left=100, top=100, right=466, bottom=612
    // you can also define percentage values of screen or relative positions, see wiki
    );

  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate,
    @gCaptureIdeEventsObject.OnActiveEditorChanged);
  LazarusIDE.AddHandlerOnProjectOpened(@gCaptureIdeEventsObject.OnProjectOpened);
  LazarusIDE.AddHandlerOnProjectClose(@gCaptureIdeEventsObject.OnProjectClose);
  LazarusIDE.AddHandlerOnSaveEditorFile(@gCaptureIdeEventsObject.OnFileSaved);
  LazarusIDE.AddHandlerOnIdeClose(@gCaptureIdeEventsObject.OnIdeClose);
end;


initialization
  gBookmarks := TFPList.Create;
  gBookmarksCurrentEditor:=nil;
  gBookmarksItemIndex := -1;
  gBookmarksInCurrentEditorCount := 0;
  gBookmarksCurrentEditorFilename := '';
  gBookmarksCurrentEditorFilenameIsRelative := False;
  gBookmarksCurrentEditorFilenameHash := 0;
  gBookmarksCurrentEditorUnitname := '';
  gBookMarksProjectFolder := '';
  gCaptureIdeEventsObject := TCaptureIdeEvents.Create;
//finalization
  // We cant't depend on the finalization order of units.
  // Leaking memory when used with DockedForms.
  // now I free resources in the OnIdeClose event.
end.
