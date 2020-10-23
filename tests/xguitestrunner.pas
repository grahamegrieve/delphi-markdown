{
  Copyright (C) 2004-2015 Dean Zobec, contributors

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
  
  
  Modified:
    Graeme Geldenhuys <graemeg@gmail.com>
    Darius Blaszijk <dhkblaszyk@zeelandnet.nl>
    Reinier Olislagers <reinierolislagers@gmail.com>
    Serguei Tarassov <serge@arbinada.com>
}

{
This is an exact copy of the normal GUI Test Runner, but it allows you to specify the location for the ini file (for OSX)
Also, add a stop button
}
unit xguitestrunner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, ActnList, Menus, Clipbrd, StdCtrls, LCLProc,
  IniFiles, testdecorator, xmltestreport,
  fpcunit, testregistry, SynEdit, SynHighlighterXML, gettext, Translations;

type
  TGUIXTestRunnerHandlerType = (
    gtrhtBeforeRunTest, // called at the beginning of RunTest
    gtrhtAfterRunTest // called at the end of RunTest
    );

  TGUIXTestRunner = class;

  TOnBeforeOrAfterRunTestEvent = procedure(AGUITestRunnerForm: TGUIXTestRunner) of object;

  { TGUIXTestRunner }

  TGUIXTestRunner = class(TForm, ITestListener)
    ActCloseForm: TAction;
    ActCopyErrorMsg: TAction;
    ActCheckCurrentSuite: TAction;
    ActCheckAll: TAction;
    ActStop: TAction;
    ActSaveResults: TAction;
    ActCopyTextToClipboard: TAction;
    ActRunHighlightedTest: TAction;
    ActUncheckAll: TAction;
    ActUncheckCurrentSuite: TAction;
    ilNodeStates: TImageList;
    MainMenu1: TMainMenu;
    MemoLog: TMemo;
    MemoDetails: TMemo;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemTestTree: TMenuItem;
    MenuItemActions: TMenuItem;
    miExpandNodes: TMenuItem;
    miCollapseNodes: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miRunTest: TMenuItem;
    miShowfailureMsg: TMenuItem;
    pbBar: TPaintBox;
    PopupResults: TPopupMenu;
    RunAction: TAction;
    ActionListMain: TActionList;
    Splitter2: TSplitter;
    TestTreeImageList: TImageList;
    MainImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItemCopyText: TMenuItem;
    PopupTree: TPopupMenu;
    PopupDetails: TPopupMenu;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    TestTree: TTreeView;
    SynXMLSyn1: TSynXMLSyn;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    btnStop: TToolButton;
    tsTestTree: TTabSheet;
    tsResultsXML: TTabSheet;
    XMLSynEdit: TSynEdit;
    actNextError: TAction;
    MenuItem20: TMenuItem;
    actPrevError: TAction;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    procedure ActCheckAllExecute(Sender: TObject);
    procedure ActCheckCurrentSuiteExecute(Sender: TObject);
    procedure ActCloseFormExecute(Sender: TObject);
    procedure ActCopyTextToClipboardExecute(Sender: TObject);
    procedure ActCopyTextToClipboardUpdate(Sender: TObject);
    procedure ActSaveResultsExecute(Sender: TObject);
    procedure ActRunHighlightedTestExecute(Sender: TObject);
    procedure ActStopExecute(Sender: TObject);
    procedure ActUncheckAllExecute(Sender: TObject);
    procedure ActRunHighLightedTestUpdate(Sender: TObject);
    procedure ActUncheckCurrentSuiteExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miCollapseNodesClick(Sender: TObject);
    procedure miExpandNodesClick(Sender: TObject);
    procedure RunExecute(Sender: TObject);
    procedure GUITestRunnerCreate(Sender: TObject);
    procedure GUITestRunnerShow(Sender: TObject);
    procedure TestTreeChange(Sender: TObject; Node: TTreeNode);
    procedure TestTreeCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TestTreeMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure TestTreeSelectionChanged(Sender: TObject);
    procedure ActCopyErrorMsgExecute(Sender: TObject);
    procedure ActCopyErrorMsgUpdate(Sender: TObject);
    procedure pbBarPaint(Sender: TObject);
    procedure actNextErrorExecute(Sender: TObject);
    procedure actPrevErrorExecute(Sender: TObject);
  private
    failureCounter: Integer;
    errorCounter: Integer;
    testsCounter: Integer;
    skipsCounter: Integer;
    barColor: TColor;
    testSuite: TTest;
    FFirstFailure: TTreeNode; // reference to first failed test
    FConfStore: TIniFile;
    FWantStop : boolean;
    procedure BuildTree(rootNode: TTreeNode; aSuite: TTestSuite);
    procedure ClearDetails;
    function  FindNode(aTest: TTest): TTreeNode;
    function GetFilename: String;
    function MakeTestPath(Node: TTreeNode): string;
    procedure ResetNodeColors;
    procedure PaintNodeError(aNode: TTreeNode);
    procedure PaintNodeFailure(aNode: TTreeNode);
    procedure PaintNodeIgnore(aNode: TTreeNode);
    procedure PaintNodeNonFailed(aNode: TTreeNode);
    procedure PaintNodeBusy(aNode: TTreeNode);
    procedure DoMemoLog(LogEntry: string);
    procedure EnableRunActions(AValue: boolean);
    procedure RestoreTree;
    procedure SaveTree;
    procedure SetFileName(AValue: String);
    procedure ShowDetails(const Node: TTreeNode);
    class procedure AddHandler(HandlerType: TGUIXTestRunnerHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    class procedure RemoveHandler(HandlerType: TGUIXTestRunnerHandlerType;
                            const AMethod: TMethod);
  protected
    class var FGuiTestRunnerHandlers: array[TGUIXTestRunnerHandlerType] of TMethodList;
  public
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure RunTest(ATest: TTest); virtual;
    procedure StartTestSuite({%H-}ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
    procedure NextError;
    procedure PrevError;
    property FileName : String read GetFilename write SetFileName;
  public
    class destructor Destroy;
    class procedure AddHandlerBeforeRunTest(const OnBeforeRunTest: TOnBeforeOrAfterRunTestEvent;
                                      AsLast: boolean = false);
    class procedure RemoveHandlerBeforeRunTest(const OnBeforeRunTest: TOnBeforeOrAfterRunTestEvent);
    class procedure AddHandlerAfterRunTest(const OnAfterRunTest: TOnBeforeOrAfterRunTestEvent;
                                     AsLast: boolean = false);
    class procedure RemoveHandlerAfterRunTest(const OnAfterRunTest: TOnBeforeOrAfterRunTestEvent);
  end;

var
  TestRunner: TGUIXTestRunner;

resourcestring
  rsAllTests = 'All Tests';
  rsRun = 'Run ';
  rsRuns = 'Runs: %s/%s';
  rsErrors = '%s    Errors: %s';
  rsFailures = '%s     Failures: %s';
  rsMessage = 'Message: %s';
  rsException = 'Exception: %s';
  rsExceptionMes = 'Exception message: %s';
  rsExceptionCla = 'Exception class: %s';
  rsUnitName = 'Unit name: %s';
  rsMethodName = 'Method name: %s';
  rsLineNumber = 'Line number: %s';
  rsRunning = 'Running %s';
  rsNumberOfExec = 'Number of executed tests: %s  Time elapsed: %s';
  // Visual components captions
  sfrmGUITest = 'FPCUnit - run unit test';
  sbtnRun = 'Run';
  sbtnRunH = 'Run highlighted test';
  sbtnClose = 'Close';
  stshTree = 'Testcase tree';
  stshResults = 'Results XML';
  sactRunAction = '&Run all';
  sactRunActionH = 'Run all checked tests';
  sactCloseForm = 'Quit';
  sactCloseFormH = 'Quit testing';
  sactCheckCurrentSuite = 'Select current suite';
  sactUncheckCurrentSuite = 'Deselect current suite';
  sactCheckAll = 'Select all tests';
  sactUncheckAll = 'Deselect all tests';
  sactRunHighlightedTest = 'Run selected';
  sactRunHighlightedTestH = 'Run selected test';
  smiActions = 'Actions';
  smiTestTree = 'Test tree';
  smiEdit = 'Edit';
  sactCopyAllToClipboard = 'Copy text to clipboard';
  sactCopyAllToClipboardH = 'Copy the entire text to clipboard';
  sactSaveResults = 'Save results';
  sactSaveResultsH = 'Save XML results to file';

implementation

{$R *.lfm}

uses
  xmlwrite;

const
  // TestTreeImageList indexes:
  imgGreenBall = 0; //success result
  imgRedBall = 2;
  imgPurpleBall = 3;
  imgWarningSign = 4; //failure result
  imgInfoSign = 11; //error result
  imgGrayBall = 12; //default
  imgBlueBall = 13; //busy

const
  SectionName_TestNodes = 'Tests';

type
  TTreeNodeState=(tsUnChecked, tsChecked);

type

  { TMessageTreeNode }

  TMessageTreeNode = class(TTreeNode)
  private
    FMessage: string;
  public
    property Message: string read FMessage write FMessage;
  end;

function FirstLine(const s: string): string;
var
  NewLinePos: integer;
begin
  NewLinePos := pos(LineEnding, s);
  if NewLinePos > 0 then
    Result := copy(s, 1, NewLinePos-1)
  else
    Result := s;
end;

{ TGUIXTestRunner }


function TGUIXTestRunner.MakeTestPath(Node: TTreeNode): string;
begin
  Result := Node.Text;
  Node := Node.Parent;
  If Node = Nil then Exit;
  // We can now skip the unnecessary "All Tests" text
  while Node.Parent <> nil do
  begin
    Result := Node.Text + '.' + Result;
    Node := Node.Parent;
  end;
end;


procedure TGUIXTestRunner.SaveTree;

  function SkipNode(const Node: TTreeNode): boolean;
  begin
    Result := Node.Data = nil;
  end;

var
  i: integer;
begin
  FConfStore.CacheUpdates := true;
  FConfStore.EraseSection(SectionName_TestNodes);
  for i := 0 to TestTree.Items.Count-1 do
  begin
    if SkipNode(TestTree.Items[i]) then
      continue;
    FConfStore.WriteBool(SectionName_TestNodes,
      MakeTestPath(TestTree.Items[i]) + '.Checked',
      TestTree.Items[i].StateIndex = Ord(tsChecked));
    FConfStore.WriteBool(SectionName_TestNodes,
      MakeTestPath(TestTree.Items[i]) + '.Expanded',
      TestTree.Items[i].Expanded);
  end;
  FConfStore.UpdateFile;
end;

procedure TGUIXTestRunner.SetFileName(AValue: String);
begin
  FConfStore := TIniFile.create(aValue);
end;

procedure TGUIXTestRunner.RestoreTree;
var
  i: integer;
begin
  if not FConfStore.SectionExists(SectionName_TestNodes) then
    Exit;
  for i := 0 to TestTree.Items.Count - 1 do
  begin
    TestTree.Items[i].Expanded := FConfStore.ReadBool(SectionName_TestNodes,
      MakeTestPath(TestTree.Items[i]) + '.Expanded',
      TestTree.Items[i].Expanded);
    if FConfStore.ReadBool(SectionName_TestNodes,
         MakeTestPath(TestTree.Items[i]) + '.Checked',
         true) then
      TestTree.Items[i].StateIndex := Ord(tsChecked)
    else
      TestTree.Items[i].StateIndex := Ord(tsUnChecked);
  end;
end;

procedure TGUIXTestRunner.GUITestRunnerCreate(Sender: TObject);
begin
  FConfStore := TIniFile.Create(ExtractFileNameOnly(ParamStr(0)) + '.fpcunit.ini'); // Prevent ini file names conflict if tests are embedded in application
  barColor := clGreen;
  TestTree.Items.Clear;
  BuildTree(TestTree.Items.AddObject(nil, rsAllTests, GetTestRegistry),
    GetTestRegistry);
  RestoreTree;
  PageControl1.ActivePage := tsTestTree;
  //
  tsTestTree.Caption:= stshTree;
  tsResultsXML.Caption:= stshResults;
  //
  Caption:= sfrmGUITest;
  RunAction.Caption:= sactRunAction;
  RunAction.Hint:= sactRunActionH;
  ActCloseForm.Caption:= sactCloseForm;
  ActCloseForm.Hint:= sactCloseFormH;
  ActCheckCurrentSuite.Caption:= sactCheckCurrentSuite;
  ActUncheckCurrentSuite.Caption:= sactUncheckCurrentSuite;
  ActCheckAll.Caption:= sactCheckAll;
  ActUncheckAll.Caption:= sactUncheckAll;
  ActRunHighlightedTest.Caption:= sactRunHighlightedTest;
  ActRunHighlightedTest.Hint := sactRunHighlightedTestH;
  MenuItemActions.Caption := smiActions;
  MenuItemTestTree.Caption := smiTestTree;
  MenuItemEdit.Caption := smiEdit;
  ActCopyTextToClipboard.Caption := sactCopyAllToClipboard;
  ActCopyTextToClipboard.Hint := sactCopyAllToClipboardH;
  ActSaveResults.Caption := sactSaveResults;
  ActSaveResults.Hint := sactSaveResultsH;

  // Select the first entry in the tree in order to immediately activate the
  // Run All tests button:
  if TestTree.Items.Count>0 then
  begin
    TestTree.Items.SelectOnlyThis(TestTree.Items[0]);
    TestTree.Items[0].Expand(False);
  end;
end;

procedure TGUIXTestRunner.RunExecute(Sender: TObject);
begin
  FFirstFailure := nil;
  testSuite := GetTestRegistry;
  TestTree.Selected := TestTree.Items[0];
  RunTest(testSuite);
end;

procedure TGUIXTestRunner.ActCloseFormExecute(Sender: TObject);
begin
  Close;
end;

procedure TGUIXTestRunner.ActCopyTextToClipboardExecute(Sender: TObject);
begin
  if ActiveControl = MemoDetails then
    Clipboard.AsText := MemoDetails.Lines.Text
  else if ActiveControl = XMLSynEdit then
    Clipboard.AsText := XMLSynEdit.Text;
end;

procedure TGUIXTestRunner.ActCopyTextToClipboardUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ActiveControl = MemoDetails) or (ActiveControl = XMLSynEdit);
end;

procedure TGUIXTestRunner.ActSaveResultsExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    XMLSynEdit.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TGUIXTestRunner.ActCheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count -1 do
    TestTree.Items[i].StateIndex := ord(tsChecked);
end;

procedure TGUIXTestRunner.ActCheckCurrentSuiteExecute(Sender: TObject);
var
  i: integer;
begin
  if (TestTree.Selected <> nil) and (TestTree.Selected.Data <> nil) then
  begin
    TestTree.Selected.StateIndex := ord(tsChecked);
    for i := 0 to TestTree.Selected.Count - 1 do
      TestTree.Selected.Items[i].StateIndex := ord(tsChecked);
  end;
end;

procedure TGUIXTestRunner.ActRunHighlightedTestExecute(Sender: TObject);
begin
  FFirstFailure := nil;
  if (TestTree.Selected <> nil) and (TestTree.Selected.Data <> nil) then
  begin
    testSuite := TTest(TestTree.Selected.Data);
  end;
  RunTest(testSuite);
  TestTree.MakeSelectionVisible;
end;

procedure TGUIXTestRunner.ActStopExecute(Sender: TObject);
begin
  FWantStop := true;
end;

procedure TGUIXTestRunner.ActUncheckAllExecute(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count -1 do
    TestTree.Items[i].StateIndex := ord(tsUnChecked);
end;


procedure TGUIXTestRunner.ActRunHighLightedTestUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ((TestTree.Selected <> nil)
    and (TestTree.Selected.Data <> nil));
end;

procedure TGUIXTestRunner.ActUncheckCurrentSuiteExecute(Sender: TObject);
var
  i: integer;
begin
  if (TestTree.Selected <> nil) and (TestTree.Selected.Data <> nil) then
  begin
    TestTree.Selected.StateIndex := ord(tsUnchecked);
    for i := 0 to TestTree.Selected.Count - 1 do
      TestTree.Selected.Items[i].StateIndex := ord(tsUnChecked);
  end;
end;

procedure TGUIXTestRunner.FormDestroy(Sender: TObject);
begin
  // store window position and size
  FConfStore.WriteInteger('WindowState', 'Left', Left);
  FConfStore.WriteInteger('WindowState', 'Top', Top);
  FConfStore.WriteInteger('WindowState', 'Width', Width);
  FConfStore.WriteInteger('WindowState', 'Height', Height);
  SaveTree;
  FConfStore.Free;
end;

procedure TGUIXTestRunner.miCollapseNodesClick(Sender: TObject);
begin
  if not Assigned(TestTree.Selected) then
    Exit;
  TestTree.Selected.Collapse(True);
end;

procedure TGUIXTestRunner.miExpandNodesClick(Sender: TObject);
begin
  if not Assigned(TestTree.Selected) then
    Exit;
  TestTree.Selected.Expand(True);
end;

procedure TGUIXTestRunner.GUITestRunnerShow(Sender: TObject);
begin
  // restore last used position and size
  Left := FConfStore.ReadInteger('WindowState', 'Left', Left);
  Top := FConfStore.ReadInteger('WindowState', 'Top', Top);
  Width := FConfStore.ReadInteger('WindowState', 'Width', Width);
  Height := FConfStore.ReadInteger('WindowState', 'Height', Height);

  if (ParamStrUTF8(1) = '--now') or (ParamStrUTF8(1) = '-n') then
    RunExecute(Self);
end;

procedure TGUIXTestRunner.TestTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if not Assigned(Node) then
    Exit;
  //MemoDetails.Lines.Text := TMessageTreeNode(Node).Message;
end;

procedure TGUIXTestRunner.TestTreeCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TMessageTreeNode;
end;

procedure TGUIXTestRunner.TestTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure ChangeCheck(aNode: TTreeNode; aCheck: TTreeNodeState);
  var
    i: integer;
    n: TTreeNode;
  begin
    if Assigned(aNode) then
    begin
      aNode.StateIndex := ord(aCheck);
      if (TTest(aNode.Data) is TTestSuite) then
        for i := 0 to aNode.Count - 1 do
        begin
          n := aNode.Items[i];
          ChangeCheck(n, aCheck);
        end;
    end;
  end;

var
  ht: THitTests;
  lNode: TTreeNode;
begin
  ht := (Sender as TTreeview).GetHitTestInfoAt(X, Y);
  if htOnStateIcon in ht then
  begin
    lNode := (Sender as TTreeview).GetNodeAt(X, Y);
    case lNode.StateIndex of
        0: ChangeCheck(lNode, tsChecked);
        1: ChangeCheck(lNode, tsUnChecked);
      end;
   end;
end;

procedure TGUIXTestRunner.ClearDetails;
begin
  MemoDetails.Lines.Clear;
end;

procedure TGUIXTestRunner.ShowDetails(const Node: TTreeNode);

  procedure AddMessages(const Node: TTreeNode);
  begin
    if (Node is TMessageTreeNode) and (TMessageTreeNode(Node).Message <> '') then
      MemoDetails.Lines.Add(TMessageTreeNode(Node).Message)
    else
      MemoDetails.Lines.Add(Node.Text);
  end;

var
  CurrNode: TTreeNode;
begin
  ClearDetails;
  if (Node.Data <> nil) and (TObject(Node.Data) is TTestCase) then
  begin
    CurrNode := Node.GetFirstChild;
    while CurrNode <> nil do
    begin
      AddMessages(CurrNode);
      CurrNode := CurrNode.GetNextSibling;
    end;
  end
  else if (Node.Parent <> nil) and (Node.Parent.Data <> nil) and (TObject(Node.Parent.Data) is TTestCase) then
    AddMessages(Node);
end;

class procedure TGUIXTestRunner.AddHandler(
  HandlerType: TGUIXTestRunnerHandlerType; const AMethod: TMethod;
  AsLast: boolean);
begin
  if FGuiTestRunnerHandlers[HandlerType]=nil then
    FGuiTestRunnerHandlers[HandlerType]:=TMethodList.Create;
  FGuiTestRunnerHandlers[HandlerType].Add(AMethod,AsLast);
end;

class procedure TGUIXTestRunner.RemoveHandler(
  HandlerType: TGUIXTestRunnerHandlerType; const AMethod: TMethod);
begin
  FGuiTestRunnerHandlers[HandlerType].Remove(AMethod);
end;

procedure TGUIXTestRunner.TestTreeSelectionChanged(Sender: TObject);
begin
  if (Sender as TTreeView).Selected <> nil then
    ShowDetails((Sender as TTreeView).Selected);
end;


procedure TGUIXTestRunner.ActCopyErrorMsgExecute(Sender: TObject);
begin
  ClipBoard.AsText := (TestTree.Selected as TMessageTreeNode).Message;
end;


procedure TGUIXTestRunner.ActCopyErrorMsgUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(TestTree.selected) and
    (Copy(TestTree.Selected.Text, 1, 9) = 'Message: ');
end;


procedure TGUIXTestRunner.pbBarPaint(Sender: TObject);
var
  msg: string;
  alltests: integer;
  OldStyle: TBrushStyle;
begin
  with (Sender as TPaintBox) do
  begin
    Canvas.Lock;
    Canvas.Brush.Color := clSilver;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Font.Color := clWhite;
    if Assigned(TestSuite) then
    begin
      alltests := TestSuite.CountTestCases;
      if alltests - skipsCounter <> 0 then
      begin
        if FailureCounter + ErrorCounter = 0 then
          barColor := clGreen;
        Canvas.Brush.Color := barColor;
        Canvas.Rectangle(0, 0, round(TestsCounter / (alltests - skipsCounter) * Width), Height);
        msg := Format(rsRuns, [IntToStr(TestsCounter), IntToStr(alltests -
          skipsCounter)]);
        msg := Format(rsErrors, [msg, IntToStr(ErrorCounter)]);
        msg := Format(rsFailures, [msg, IntToStr(FailureCounter)]);
        OldStyle := Canvas.Brush.Style;
        Canvas.Brush.Style := bsClear;
        Canvas.Textout(10, 10,  msg);
        Canvas.Brush.Style := OldStyle;
      end;
    end;
    Canvas.UnLock;
  end;
end;

procedure TGUIXTestRunner.actNextErrorExecute(Sender: TObject);
begin
  NextError;
end;

procedure TGUIXTestRunner.actPrevErrorExecute(Sender: TObject);
begin
  PrevError;
end;


procedure TGUIXTestRunner.BuildTree(rootNode: TTreeNode; aSuite: TTestSuite);
var
  node: TTreeNode;
  i: integer;
begin
  rootNode.StateIndex := Ord(tsChecked);
  for i := 0 to ASuite.ChildTestCount - 1 do
  begin
    node := TestTree.Items.AddChildObject(rootNode, ASuite.Test[i].TestName, ASuite.Test[i]);
    if ASuite.Test[i] is TTestSuite then
      BuildTree(Node, TTestSuite(ASuite.Test[i]))
    else
      if TObject(ASuite.Test[i]).InheritsFrom(TTestDecorator) then
        BuildTree(Node, TTestSuite(TTestDecorator(ASuite.Test[i]).Test));
    node.ImageIndex := imgGrayBall;
    node.SelectedIndex := imgGrayBall;
    node.StateIndex := ord(tsChecked);
  end;
  ResetNodeColors;
end;


function TGUIXTestRunner.FindNode(aTest: TTest): TTreeNode;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to TestTree.Items.Count -1 do
    if (TTest(TestTree.Items[i].Data) = aTest) then
    begin
      Result :=  TestTree.Items[i];
      Exit;
    end;
end;

function TGUIXTestRunner.GetFilename: String;
begin
  result := FConfStore.FileName;
end;


procedure TGUIXTestRunner.ResetNodeColors;
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count - 1 do
  begin
    TestTree.Items[i].ImageIndex := imgGrayBall;
    TestTree.Items[i].SelectedIndex := imgGrayBall;
  end;
end;


procedure TGUIXTestRunner.PaintNodeError(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    aNode.ImageIndex := imgRedBall;
    aNode.SelectedIndex := imgRedBall;
    if aNode.AbsoluteIndex<>0 then begin
      aNode.Expand(True);
    end;
    aNode := aNode.Parent;
  end;
end;


procedure TGUIXTestRunner.PaintNodeFailure(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    if ((aNode.ImageIndex in [imgGreenBall, imgGrayBall, imgBlueBall]) or
      (ANode.ImageIndex = -1)) then
    begin
      aNode.ImageIndex := imgPurpleBall;
      aNode.SelectedIndex := imgPurpleBall;
      if aNode.AbsoluteIndex<>0 then begin
        aNode.Expand(true);
      end;
    end;
    aNode := aNode.Parent;
  end;
end;

procedure TGUIXTestRunner.PaintNodeIgnore(aNode: TTreeNode);
// Test results with Ignore
var
  noFailedSibling: boolean;
  i: integer;
begin
  if Assigned(aNode) then
  begin
    if ((aNode.ImageIndex in [imgGrayBall, imgBlueBall]) or
      (ANode.ImageIndex = -1)) then
    begin
      aNode.ImageIndex := imgGreenBall;
      aNode.SelectedIndex := imgGreenBall;
    end;
  end;
  if Assigned(aNode.Parent) then
    if aNode.Index = aNode.Parent.Count -1 then
    begin
    aNode := aNode.Parent;
    noFailedSibling := true;
    for i := 0 to aNode.Count -2 do
    begin
      if aNode.Items[i].ImageIndex <> imgGreenBall then
        noFailedSibling := false;;
    end;
    if (aNode.ImageIndex = imgBlueBall) and
      noFailedSibling then
      PaintNodeIgnore(aNode);
    end;
end;


procedure TGUIXTestRunner.PaintNodeNonFailed(aNode: TTreeNode);
var
  noFailedSibling: boolean;
  i: integer;
begin
  if Assigned(aNode) then
  begin
    if ((aNode.ImageIndex in [imgGrayBall, imgBlueBall]) or
      (ANode.ImageIndex = -1)) then
    begin
      aNode.ImageIndex := imgGreenBall;
      aNode.SelectedIndex := imgGreenBall;
    end;
  end;
  if Assigned(aNode.Parent) then
    if aNode.Index = aNode.Parent.Count -1 then   // is aNode the last child
    begin
    aNode := aNode.Parent;
    noFailedSibling := true;
    for i := 0 to aNode.Count -2 do
    begin
      if aNode.Items[i].ImageIndex <> imgGreenBall then
        noFailedSibling := false;
    end;
    if (aNode.ImageIndex = imgBlueBall) and
      noFailedSibling then
      PaintNodeNonFailed(aNode);
    end;
end;


procedure TGUIXTestRunner.PaintNodeBusy(aNode: TTreeNode);
var
  BusySibling: boolean;
  i: integer;
begin
  if Assigned(aNode) then
  begin
    aNode.ImageIndex := imgBlueBall;
    aNode.SelectedIndex := imgBlueBall;
  end;
  if Assigned(aNode.Parent) then
  begin
    if aNode.Index = aNode.Parent.Count -1 then
    begin
      aNode := aNode.Parent;
      BusySibling := true;
      for i := 0 to aNode.Count -2 do
      begin
        if aNode.Items[i].ImageIndex <> imgGreenBall then
          BusySibling := false;
      end;
      if (aNode.ImageIndex = imgBlueBall) and BusySibling then
        PaintNodeBusy(aNode);
    end;
  end;
end;

procedure TGUIXTestRunner.DoMemoLog(LogEntry: string);
begin
  MemoLog.Lines.Add(TimeToStr(Now) + ' - ' + LogEntry);
end;

procedure TGUIXTestRunner.EnableRunActions(AValue: boolean);
begin
  ActRunHighlightedTest.Enabled := AValue;
  RunAction.Enabled := AValue;
  actStop.enabled := not AValue;
end;

procedure TGUIXTestRunner.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  FailureNode: TTreeNode;
  Node: TMessageTreeNode;
begin
  FailureNode := FindNode(ATest);
  if Assigned(FailureNode) then
  begin
    Node := TestTree.Items.AddChild(FailureNode,
      Format(rsMessage, [FirstLine(AFailure.ExceptionMessage)]))
      as TMessageTreeNode;
    if not(AFailure.IsIgnoredTest) then
    begin
      // Genuine failure
      if not Assigned(FFirstFailure) then
        FFirstFailure := FailureNode;
      Node.Message := AFailure.ExceptionMessage;
      Node.ImageIndex := imgWarningSign;
      Node.SelectedIndex := imgWarningSign;
      Node := TestTree.Items.AddChild(FailureNode,
        Format(rsException, [AFailure.ExceptionClassName])) as TMessageTreeNode;
      Node.ImageIndex := imgWarningSign;
      Node.SelectedIndex := imgWarningSign;
      {$IF FPC_FULLVERSION <= 30001}
      Node := TestTree.Items.AddChild(FailureNode,
        Format('at line %d in <%s>', [AFailure.LineNumber, AFailure.UnitName])) as TMessageTreeNode;
      {$ELSE}
      Node := TestTree.Items.AddChild(FailureNode, 'at ' + AFailure.LocationInfo) as TMessageTreeNode;
      {$ENDIF}
      Node.ImageIndex := imgWarningSign;
      Node.SelectedIndex := imgWarningSign;
      PaintNodeFailure(FailureNode);
    end
    else
    begin
      // Although reported as a failure, the test was set up
      // to be ignored so it is actually a success of sorts
      Node.Message := AFailure.ExceptionMessage;
      Node.ImageIndex := imgGreenBall;
      Node.SelectedIndex := imgGreenBall;
      Node := TestTree.Items.AddChild(FailureNode,
        Format(rsException, [AFailure.ExceptionClassName])) as TMessageTreeNode;
      Node.ImageIndex := imgGreenBall;
      Node.SelectedIndex := imgGreenBall;
      PaintNodeIgnore(FailureNode);
    end;
    ShowDetails(FailureNode);
  end;

  if not(AFailure.IsIgnoredTest) then
  begin
    Inc(failureCounter);
    if errorCounter = 0 then
      barColor := clFuchsia;
  end;
end;


procedure TGUIXTestRunner.AddError(ATest: TTest; AError: TTestFailure);
var
  ErrorNode, Node: TTreeNode;
  MessageNode: TMessageTreeNode;
begin
  ErrorNode := FindNode(ATest);
  if Assigned(ErrorNode) then
  begin
    if not Assigned(FFirstFailure) then
      FFirstFailure := ErrorNode;
    MessageNode := TestTree.Items.AddChild(ErrorNode,
      Format(rsExceptionMes, [FirstLine(AError.ExceptionMessage)]))
      as TMessageTreeNode;
    MessageNode.Message := AError.ExceptionMessage;
    MessageNode.ImageIndex := imgWarningSign;
    MessageNode.SelectedIndex := imgWarningSign;
    Node := TestTree.Items.AddChild(ErrorNode, Format(rsExceptionCla, [
      AError.ExceptionClassName]));
    Node.ImageIndex := imgWarningSign;
    Node.SelectedIndex := imgWarningSign;
    // line info details
    {$IF FPC_FULLVERSION <= 30001}
    Node := TestTree.Items.AddChild(ErrorNode,
      Format('at line %d in <%s>', [AError.LineNumber, AError.UnitName])) as TMessageTreeNode;

    {$ELSE}
    node := TestTree.Items.AddChild(ErrorNode, 'at ' + AError.LocationInfo);
    {$ENDIF}
    Node.ImageIndex := imgInfoSign;
    Node.SelectedIndex := imgInfoSign;
    // TODO : add stack trace info

    PaintNodeError(ErrorNode);
    ShowDetails(ErrorNode);
  end;
  Inc(errorCounter);
  barColor := clRed;
end;


procedure TGUIXTestRunner.StartTest(ATest: TTest);
var
  Node: TTreeNode;
begin
  TestTree.BeginUpdate;
  Node := FindNode(ATest);
  Node.DeleteChildren;
  PaintNodeBusy(Node);
  if Node.Level=1 then begin
    Node.MakeVisible;
  end;
  if assigned(Node.Parent) and (Node.Parent.Level=1) then begin
    Node.Parent.MakeVisible;
  end;
  Application.ProcessMessages;
  TestTree.EndUpdate;
end;


procedure TGUIXTestRunner.EndTest(ATest: TTest);
var
  Node: TTreeNode;
begin
  TestTree.BeginUpdate;
  Inc(testsCounter);
  Node := FindNode(ATest);
  PaintNodeNonFailed(Node);
  pbbar.Refresh;
  Application.ProcessMessages;
  TestTree.EndUpdate;
  if FWantStop then
    Abort;
end;

procedure TGUIXTestRunner.RunTest(ATest: TTest);

  procedure SkipUncheckedTests(aResult: TTestResult; aNode: TTreeNode);
  var
    i: integer;
  begin
    if (aNode.StateIndex = ord(tsUnChecked)) and (TTest(aNode.Data) is TTestCase) then
      aResult.AddToSkipList(TTest(aNode.Data) as TTestCase);
    for i := 0 to aNode.Count - 1 do
      SkipUncheckedTests(aResult, aNode.Items[i]);
  end;

var
  TestResult:TTestResult;
  w: TXMLResultsWriter;
  m: TMemoryStream;
  xMethod: TOnBeforeOrAfterRunTestEvent;
  I: integer;
begin
  if Assigned(FGuiTestRunnerHandlers[gtrhtBeforeRunTest]) then
  begin
    for I := 0 to FGuiTestRunnerHandlers[gtrhtBeforeRunTest].Count-1 do
    begin
      xMethod := TOnBeforeOrAfterRunTestEvent(FGuiTestRunnerHandlers[gtrhtBeforeRunTest][i]);
      xMethod(Self);
    end;
  end;
  SaveTree;
  ClearDetails;
  barcolor := clGreen;
  ResetNodeColors;
  failureCounter := 0;
  errorCounter := 0;
  testsCounter := 0;
  skipsCounter := 0;
  FWantStop := false;
  EnableRunActions(false);
  TestResult := TTestResult.Create;
  try
    SkipUncheckedTests(TestResult, TestTree.Selected);
    skipsCounter := TestResult.NumberOfSkippedTests;
    TestResult.AddListener(self);
    pbBar.Invalidate;
    w := TXMLResultsWriter.Create(nil);
    try
      w.FileName := 'null'; // prevents output to the console
      TestResult.AddListener(w);

      DoMemoLog(Format(rsRunning, [TestTree.Selected.Text]));
      aTest.Run(TestResult);
      DoMemoLog(Format(rsNumberOfExec, [IntToStr(TestResult.RunTests),
      FormatDateTime('hh:nn:ss.zzz', Now - TestResult.StartingTime)]));

      w.WriteResult(TestResult);
      m := TMemoryStream.Create;
      try
        try
          WriteXMLFile(w.Document, m);
          m.Position := 0;
          XMLSynEdit.Lines.LoadFromStream(m);
        except
          on E: Exception do
            XMLSynEdit.Lines.Text:='WriteXMLFile exception: ' + E.ClassName + '/' + E.Message;
        end;
      finally
        m.Free;
      end;
      pbBar.Invalidate;
    finally
      w.Free;
    end;
  finally
    EnableRunActions(true);
    TestResult.Free;
    if Assigned(FGuiTestRunnerHandlers[gtrhtAfterRunTest]) then
    begin
      for I := 0 to FGuiTestRunnerHandlers[gtrhtAfterRunTest].Count-1 do
      begin
        xMethod := TOnBeforeOrAfterRunTestEvent(FGuiTestRunnerHandlers[gtrhtAfterRunTest][i]);
        xMethod(Self);
      end;
    end;
  end;
end;

procedure TGUIXTestRunner.StartTestSuite(ATestSuite: TTestSuite);
begin
  // do nothing
end;

procedure TGUIXTestRunner.EndTestSuite(ATestSuite: TTestSuite);
var
  Node: TTreeNode;
begin
  // scroll treeview to first failed test
  if Assigned(FFirstFailure) then
  begin
    TestTree.Selected := FFirstFailure;
    TestTree.MakeSelectionVisible;
  end;

  Node := FindNode(ATestSuite);
  if Assigned(Node) then
    PaintNodeNonFailed(Node);
end;

procedure TGUIXTestRunner.NextError;
var
  Node: TTreeNode;
begin
  Node := TestTree.Selected;
  while Assigned(Node) do
  begin
    Node := Node.GetNext;
    if Assigned(Node) and (Node.ImageIndex in [imgRedBall, imgPurpleBall]) and
      (TObject(Node.Data) is TTestCase) then
    begin
      TestTree.Selected := Node;
      TestTree.MakeSelectionVisible;
      Exit;
    end;
  end;
end;

procedure TGUIXTestRunner.PrevError;
var
  Node: TTreeNode;
begin
  Node := TestTree.Selected;
  while Assigned(Node) do
  begin
    Node := Node.GetPrev;
    if Assigned(Node) and (Node.ImageIndex in [imgRedBall, imgPurpleBall]) and
      (TObject(Node.Data) is TTestCase) then
    begin
      TestTree.Selected := Node;
      TestTree.MakeSelectionVisible;
      Exit;
    end;
  end;
end;

class destructor TGUIXTestRunner.Destroy;
var
  HandlerType: TGUIXTestRunnerHandlerType;
begin
  for HandlerType := Low(TGUIXTestRunnerHandlerType) to High(TGUIXTestRunnerHandlerType) do
    FreeAndNil(FGuiTestRunnerHandlers[HandlerType]);
end;

class procedure TGUIXTestRunner.AddHandlerBeforeRunTest(
  const OnBeforeRunTest: TOnBeforeOrAfterRunTestEvent; AsLast: boolean);
begin
  AddHandler(gtrhtBeforeRunTest,TMethod(OnBeforeRunTest),AsLast);
end;

class procedure TGUIXTestRunner.RemoveHandlerBeforeRunTest(
  const OnBeforeRunTest: TOnBeforeOrAfterRunTestEvent);
begin
  RemoveHandler(gtrhtBeforeRunTest,TMethod(OnBeforeRunTest));
end;

class procedure TGUIXTestRunner.AddHandlerAfterRunTest(
  const OnAfterRunTest: TOnBeforeOrAfterRunTestEvent; AsLast: boolean);
begin
  AddHandler(gtrhtAfterRunTest,TMethod(OnAfterRunTest),AsLast);
end;

class procedure TGUIXTestRunner.RemoveHandlerAfterRunTest(
  const OnAfterRunTest: TOnBeforeOrAfterRunTestEvent);
begin
  RemoveHandler(gtrhtAfterRunTest,TMethod(OnAfterRunTest));
end;

procedure TranslateResStrings;
var
  Lang, FallbackLang, S: String;
begin
  Lang:='';
  FallbackLang:='';
  GetLanguageIDs(Lang, FallbackLang); // in unit gettext
  S := AppendPathDelim(AppendPathDelim(ExtractFileDir(ParamStr(0))) + 'languages');
  if FallbackLang = 'pt' then
     Lang := 'pb';
  TranslateUnitResourceStrings('guitestrunner', S + 'guitestrunner.%s.po', Lang, FallbackLang);
end;

initialization
  TranslateResStrings;

end.

