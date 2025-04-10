const
  constKindPcb = 'PCB';

  // GLOBAL VARIABLES SECTION
Var
  CurrWorkSpace: IWorkSpace; // An Interface handle to the current workspace
  CurrProject: IProject; // An Interface handle to the current Project
  ProjectVariant: IProjectVariant; // An Interface handle to the current Variant
  FlattenedDoc: IDocument; // An Interface handle to the "flattened" document
  CurrComponent: IComponent; // An Interface handle to the current Component
  TargetFileName: String;
  // System parameter supplied by OutJob : desired output file name, if any
  TargetFolder: String;
  // System parameter supplied by OutJob : desired output folder
  TargetPrefix: String;
  // System parameter supplied by OutJob : desired output file name prefix, if any
  LayerFilterIndex: Integer;
  FormatIndex: Integer;
  FieldSeparatorIndex: Integer;
  PluginExecutable: String;
  DarkMode: Boolean;
  AddNets: Boolean;
  AddTracks: Boolean;
  Highlighting1Pin: Boolean;
  FabLayer: Boolean;
  Title: String;
  Company: String;
  Revision: String;
  ValueParameterName: String;
  ColumnsParametersNames: TStringList;
  GroupParametersNames: TStringList;
  RunAsOutputJob: Boolean;


procedure SetupProjectVariant; forward;
Function GetBoard: IPCB_Board; forward;
function GenerateNativeConfig: String; forward;
procedure PopulateChoiceFields; forward;
procedure PopulateStaticFields; forward;
procedure PopulateDynamicFields(Board: IPCB_Board); forward;
Procedure InitializeProject; forward;
function GetSelectedFields: TStringList; forward;
function GetSelectedGroupParameters: TStringList; forward;

{.....................................................................................................................}
{.                                              Global Variable Mapping                                              .}
{.....................................................................................................................}


function GetSeparator: String;
var
  Separator: TString;
begin
  case FieldSeparatorIndex of
    0:
      Separator := ',';
    1:
      Separator := ';';
    2:
      Separator := ' ';
    3:
      Separator := #9;
    // else
  end;

  Result := Separator;
end;


{.....................................................................................................................}
{.                                            Path and Filename Handling                                             .}
{.....................................................................................................................}


function GetPluginExecutableFileName: String;
var
  i: Integer;
  Path: String;
  CurrentDir: String;
  Document: IDocument;
begin
  Result := PluginExecutable;
  if not IsPathRelative(PluginExecutable) then
    Exit;

  Result := ExpandFileName(PluginExecutable);

  for i := 0 to CurrProject.DM_LogicalDocumentCount - 1 do
  begin
    Document := CurrProject.DM_LogicalDocuments(i);
    if Document.DM_FileName <> 'GOSTBOMAltium.pas' then
      continue;

    Path := ExtractFilePath(Document.DM_FullPath);
    if not DirectoryExists(Path) then
      continue;

    CurrentDir := GetCurrentDir;
    if not SetCurrentDir(Path) then
      continue;

    Result := ExpandFileName(PluginExecutable);
    SetCurrentDir(CurrentDir);
  end;
end;


Function GetOutputFileNameWithExtension(Ext: String): String;
Begin
  If TargetFolder = '' Then
    TargetFolder := CurrProject.DM_GetOutputPath;
  If TargetFileName = '' Then
    TargetFileName := CurrProject.DM_ProjectFileName;
  Result := AddSlash(TargetFolder) + TargetPrefix +
    ChangeFileExt(TargetFileName, Ext);
  // CurrString := StringReplace( CurrString, '<DATE>',    DateStr,    MkSet( rfReplaceAll, rfIgnoreCase ) );
End;

function GetWDFileName(FF: String): String;
var
  i: Integer;
  Path: String;
  CurrentDir: String;
  Document: IDocument;
begin
  Result := FF;
  if not IsPathRelative(FF) then
    Exit;

  Result := ExpandFileName(FF);

  for i := 0 to CurrProject.DM_LogicalDocumentCount - 1 do
  begin
    Document := CurrProject.DM_LogicalDocuments(i);
    if Document.DM_FileName <> 'InteractiveHTMLBOM4Altium2.pas' then
      continue;

    Path := ExtractFilePath(Document.DM_FullPath);
    if not DirectoryExists(Path) then
      continue;

    CurrentDir := GetCurrentDir;
    if not SetCurrentDir(Path) then
      continue;

    Result := ExpandFileName(FF);
    SetCurrentDir(CurrentDir);
  end;
end;

{ ***************************************************************************
  * function FindProjectPcbDocFile()
  *  Find the PcbDoc file associated with this project.
  *  Panic if we find any number not equal to 1 (eg 0 or 2).
  *
  *  Returns full path to this project's PcbDoc file in var parm pcbDocPath.
  *  Returns:  0 on success, 1 if not successful.
  *************************************************************************** }
function FindProjectPcbDocFile(Project: IProject;
  flagRequirePcbDocFile: Boolean; var pcbDocPath: TDynamicString;): Integer;
var
  Document: IDocument;
  k: Integer;
  numPcbDocs: Integer;

begin

  { For now, assume/hope/pray that we will succeed. }
  Result := 0;

  { Flag that we haven't yet found the PcbDoc file. }
  pcbDocPath := '';

  { Init number of PcbDoc files found to be 0. }
  numPcbDocs := 0;

  { *** Find name of this project's .PcbDoc file. *** }
  { Loop over all logical documents in the project.... }
  for k := 0 to Project.DM_LogicalDocumentCount - 1 do
  begin
    Document := Project.DM_LogicalDocuments(k);

    { See if this document is a PcbDoc file. }
    if (Document.DM_DocumentKind = constKindPcb) then
    begin

      { Increment number of PcbDoc files that we've found. }
      numPcbDocs := numPcbDocs + 1;

      { Record name of this PcbDoc file to return to caller. }
      pcbDocPath := Document.DM_FullPath;
      // ShowMessage('Found PCB document with full path ' + pcbDocPath);
    end;

  end; { endfor loop over all logical documents in project }

  { Make sure there is at least one PcbDoc file. }
  if (numPcbDocs < 1) then
  begin

    { See if the user has requested operations that require a PcbDoc file. }
    if (flagRequirePcbDocFile) then
    begin
      MyAbort('Found ' + IntToStr(numPcbDocs) +
        ' PcbDoc files in your project.  This number should have been exactly 1!');
    end

    { Else just issue a warning. }
    else
    begin
      { Issue warning modal dialog box with specified warning message,
        no reply after clicking Ok, and specified reply after clicking Cancel. }
      IssueWarningWithOkOrCancel
        ('Unable to find a PcbDoc file within this project.' + constLineBreak +
        'However, since you have not requested operations that require a PcbDoc file, I will proceed to generate other OutJob outputs if you click OK.',
        '', 'Aborting script at user request due to missing PcbDoc file ' +
        constPcbVersionParm + '.');
    end; { endelse }

  end; { endif }

  { Make sure there is no more than 1 PcbDoc file. }
  if (numPcbDocs > 1) then
  begin
    MyAbort('Found ' + IntToStr(numPcbDocs) +
      ' PcbDoc files in your project.  This script currently only supports having 1 PcbDoc file per project!');
  end;

  // ShowMessage('About to leave FindProjectPcbDocFile(), pcbDocPath is ' + pcbDocPath);

end; { end FindProjectPcbDocFile() }


{.....................................................................................................................}
{.                                               Workspace Interaction                                               .}
{.....................................................................................................................}


Function GetBoard: IPCB_Board;
Var
  Board: IPCB_Board; // document board object
  Document: IServerDocument;
  pcbDocPath: TString;
  flagRequirePcbDocFile: Boolean;
Begin
  // Make sure the current Workspace opens or else quit this script
  CurrWorkSpace := GetWorkSpace;
  If (CurrWorkSpace = Nil) Then
    Exit;

  // Make sure the currently focussed Project in this Workspace opens or else
  // quit this script
  CurrProject := CurrWorkSpace.DM_FocusedProject;
  If CurrProject = Nil Then
    Exit;

  flagRequirePcbDocFile := True;

  FindProjectPcbDocFile(CurrProject, flagRequirePcbDocFile, pcbDocPath);

  // TODO: Close
  Document := Client.OpenDocument('pcb', pcbDocPath);
  Board := PCBServer.GetPCBBoardByPath(pcbDocPath);

  If Not Assigned(Board) Then // check of active document
  Begin
    ShowMessage('The Current Document is not a PCB Document.');
    Exit;
  End;

  Result := Board;  
End;


{.....................................................................................................................}
{.                                              JSON String Conversions                                              .}
{.....................................................................................................................}

function JSONFloatToStr(v: Single): String;
begin
  Result := StringReplace(FloatToStr(v), ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));
end;

function JSONBoolToStr(v: Boolean): String;
begin
  if v then
    Result := 'true'
  else
    Result := 'false';
end;

function JSONStrToStr(v: String): String;
var
  i: Integer;
begin
  Result := '"';
  for i := 1 to Length(v) do
  begin
    case v[i] of
      '"':
        Result := Result + '\"';
      '\':
        Result := Result + '\\';
      #$08:
        Result := Result + '\b';
      #$09:
        Result := Result + '\t';
      #$0a:
        Result := Result + '\n';
      #$0b:
        Result := Result + '\v';
      #$0c:
        Result := Result + '\f';
      #$0d:
        Result := Result + '\r';
      #$00 .. #$07, #$0e .. #$1f, #$7e .. #$7f:
        begin
          Result := Result + '\u' + IntToHex(Ord(v[i]), 4);
        end;
    else
      Result := Result + '\u' + IntToHex(Ord(v[i]), 4);
    end;
  end;
  Result := Result + '"';
end;

function GetCompFromComp(pcbc: IPCB_Component): IComponent;
begin
  // Make sure the current Workspace opens or else quit this script
  CurrWorkSpace := GetWorkSpace;
  If (CurrWorkSpace = Nil) Then
    Exit;

  // Make sure the currently focussed Project in this Workspace opens or else
  // quit this script
  CurrProject := CurrWorkSpace.DM_FocusedProject;
  If CurrProject = Nil Then
    Exit;

  Result := nil;
end;

function GetFlatDoc: IDocument;
Begin
  FlattenedDoc := CurrProject.DM_DocumentFlattened;

  If (FlattenedDoc = Nil) Then
  Begin
    // First try compiling the project
    AddStringParameter('Action', 'Compile');
    AddStringParameter('ObjectKind', 'Project');
    RunProcess('WorkspaceManager:Compile');

    // Try Again to open the flattened document
    FlattenedDoc := CurrProject.DM_DocumentFlattened;
  End;

  Result := FlattenedDoc;
end;

procedure ListAllFields;
Var
  CompIndex: Integer; // An Index for pullin out components
  PhysCompCount: Integer; // A count of the number of components in document
  PhysComponent: IComponent; // An Interface handle to the current Component
  PhysCompIndex: Integer; // An index for pulling out Physical Parts
  CurrPart: IPart; // An Interface handle to the current Part of a Component
  CurrSortStr: String; // A String to hold the sorting field
  CurrPhysDesStr: String; // A String to hold the current Physical Designator
  CurrFootprintStr: String; // A String to hold the current Footprint
  CurrDescrStr: String; // A String to hold the current Description
  CurrLibRefStr: String; // A String to hold the current Library Reference
  CurrStuffedStr: String;

  // Flag for processing the rest of the designator characters
  SortIndex: Integer; // Temporary Index for chars in the Designator
  CompCount: Integer; // The Number of Components Flattened Document
  CompListIndex: Integer; // An index for strings in CompList
  i: Integer;
  _n: IParameter;
  _nn: string;
  _vv: string;
  _ss: TStringList;
  _pp: TStringList;
  ParmIndex: Integer;
  ParmCount: Integer; // The Number of Parameters in Component
  CurrParm: IParameter; // An interface handle to a Parameter
  iii: Integer;
  Line: String;
  ComponentVariation: IComponentVariation;

Begin
  FlattenedDoc := GetFlatDoc();

  CompCount := FlattenedDoc.DM_ComponentCount;

  _pp := TStringList.Create;
  _pp.Sorted := True;
  _pp.Duplicates := dupIgnore;
  For CompIndex := 0 To CompCount - 1 Do
  Begin
    CurrComponent := FlattenedDoc.DM_Components[CompIndex];
    ParmCount := CurrComponent.DM_ParameterCount;
    For ParmIndex := 0 To ParmCount - 1 Do
    Begin
      CurrParm := CurrComponent.DM_Parameters(ParmIndex);
      _pp.Add(CurrParm.DM_Name);
    End;
  end;
  _pp.Free;
end;

function GetCompFromCompEx(pcbc: IPCB_Component): IComponent;
Var
  CompIndex: Integer; // An Index for pullin out components
  PhysCompCount: Integer; // A count of the number of components in document
  PhysComponent: IComponent; // An Interface handle to the current Component
  PhysCompIndex: Integer; // An index for pulling out Physical Parts
  CurrPart: IPart; // An Interface handle to the current Part of a Component
  CurrSortStr: String; // A String to hold the sorting field
  CurrPhysDesStr: String; // A String to hold the current Physical Designator
  CurrFootprintStr: String; // A String to hold the current Footprint
  CurrDescrStr: String; // A String to hold the current Description
  CurrLibRefStr: String; // A String to hold the current Library Reference
  CurrStuffedStr: String;

  // Flag for processing the rest of the designator characters
  SortIndex: Integer; // Temporary Index for chars in the Designator
  CompCount: Integer; // The Number of Components Flattened Document
  CompListIndex: Integer; // An index for strings in CompList
  i: Integer;
  _n: IParameter;
  _nn: string;
  _vv: string;
  _ss: TStringList;
  _pp: TStringList;
  ParmIndex: Integer;
  ParmCount: Integer; // The Number of Parameters in Component
  CurrParm: IParameter; // An interface handle to a Parameter
  iii: Integer;
  Line: String;
  ComponentVariation: IComponentVariation;

Begin
  FlattenedDoc := GetFlatDoc();

  CompCount := FlattenedDoc.DM_ComponentCount;

  // Component.SourceUniqueId, Component.SourceDesignator

  For CompIndex := 0 To CompCount - 1 Do
  Begin
    CurrComponent := FlattenedDoc.DM_Components[CompIndex];
    if CurrComponent.DM_UniqueId = pcbc.SourceUniqueId then
    begin
      Result := CurrComponent;
      Exit;
    end;
  end;
end;



{.....................................................................................................................}
{.                                            Generation Helper Functions                                            .}
{.....................................................................................................................}


{
  ComponentIsFittedInCurrentVariant cross-checks the ComponentId and Designator
  against the current project variant (an @ in the ComponentId indicates that
  the component is part of a variant). Only components that are part of the
  current variation (and don't have a DNP directive) are included in the
  output.
}
Function ComponentIsFittedInCurrentVariant(ComponentId, Designator: TString;
  _ProjectVariant: IProjectVariant): Boolean;
var
  // Designator: IPart;
  ComponentVariation: IComponentVariation;
begin
  // [!!!] UGLY
  // Designator := ComponentId.DM_SubParts[0];
  if _ProjectVariant = nil then
  begin
    if pos('@', ComponentId) <> 0 then
    begin
      // Exclude components that are part of a variant but we're not inside a variant
      Result := False;
    end else begin
      // Component is not part of a variant, and we're not inside a variant
      Result := True;
    end;
  end;

  if _ProjectVariant <> nil then
  begin
    ComponentVariation := _ProjectVariant.DM_FindComponentVariationByDesignator
      (Designator);
    if ComponentVariation <> nil then
    begin
      if ComponentId <> ComponentVariation.DM_UniqueId + '@' +
        _ProjectVariant.DM_Description then
      begin
        // Exclude component that is part of another variant
        Result := False;
      end else begin
        // Include component that is part of the current variant
        Result := True;
      end;
      // [!!!] Never
      if ComponentVariation.DM_VariationKind = eVariation_NotFitted then
      begin
        // In any case, exclude components that are not fitted
        Result := False;
      end;
    end
    else
    begin
      if pos('@', ComponentId) <> 0 then
      begin
        // Component has a variant-specific ID, but there is no variation defined for it.
        // Possibly belongs to a different variant --> exclude it.
        Result := False;
      end else begin
        // Component has no variant-specific ID, and no variation is defined --> include it.
        Result := True;
      end;
    end;
  end;
end;

function GetComponentParameters(comp: IPCB_Component): TStringList;
var
  stateText: string;
  str1: string;
  paramsComponent: TStringList;
  pcb_primitiveparametersIntf: IPCB_PrimitiveParameters;
  argIndex: Integer;
  parameterByIndex: IPCB_Parameter;
  name, value: String;
begin
  stateText := comp.GetState_Name().GetState_Text();
  str1 := comp.GetState_SourceCompDesignItemID();
  paramsComponent := TStringList.Create;
  paramsComponent.Add('[DesignItemID]' + '=' + str1);
  paramsComponent.Add('[Footprint]' + '=' + comp.GetState_Pattern());
  paramsComponent.Add('[Comment]' + '=' + comp.GetState_Comment()
    .GetState_ConvertedString());
  paramsComponent.Add('[Description]' + '=' +
    comp.GetState_SourceDescription());

  begin
    pcb_primitiveparametersIntf := comp;
    for argIndex := 0 to pcb_primitiveparametersIntf.Count() - 1 do
    begin
      parameterByIndex := pcb_primitiveparametersIntf.GetParameterByIndex
        (argIndex);
      name := parameterByIndex.GetName();
      value := parameterByIndex.GetValue();
      paramsComponent.Add(name + '=' + value);
    end;
  end;
  Result := paramsComponent;
end;


{.....................................................................................................................}
{.                                               Native JSON Generation                                              .}
{.....................................................................................................................}


function ParseArc(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeRadius: String;
  EdgeType: String;
begin
  PnPout := TStringList.Create;
  // ;{edges.push(parseArc(Prim));}
  EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.LineWidth));
  EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.XCenter - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.YCenter - Board.YOrigin));
  EdgeX2 := JSONFloatToStr(-Prim.EndAngle);
  EdgeY2 := JSONFloatToStr(-Prim.StartAngle);
  EdgeRadius := JSONFloatToStr(CoordToMMs(Prim.Radius));

  EdgeType := 'arc';
  PnPout.Add('{');
  PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"Width":' + (EdgeWidth) + ',');
  PnPout.Add('"X":' + (EdgeX1) + ',');
  PnPout.Add('"Y":' + (EdgeY1) + ',');
  PnPout.Add('"Angle1":' + (EdgeX2) + ',');
  PnPout.Add('"Angle2":' + (EdgeY2) + ',');
  PnPout.Add('"Radius":' + (EdgeRadius));
  PnPout.Add('}');
  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseTrack(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeRadius: String;
  EdgeType: String;
begin
  PnPout := TStringList.Create;
  // JSONPush(edges,parseTrack(Prim));
  {
    var start = [CoordToMMs(Prim.x1).round(), -CoordToMMs(Prim.y1).round()];
    var end = [CoordToMMs(Prim.x2).round(), -CoordToMMs(Prim.y2).round()];
    res["layer"] = Prim.Layer;
    if (Prim.InPolygon)
    res["type"] = "polygon";
    res["svgpath"] = ["M", start, "L", end].join(" ");
    else             res["type"] = "segment";
    res["start"] = start;
    res["end"] = end;
    res["width"] = CoordToMMs(Prim.Width).round();
  }

  EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.Width));
  EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.X1 - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.Y1 - Board.YOrigin));
  EdgeX2 := JSONFloatToStr(CoordToMMs(Prim.X2 - Board.XOrigin));
  EdgeY2 := JSONFloatToStr(-CoordToMMs(Prim.Y2 - Board.YOrigin));

  EdgeType := 'segment';
  PnPout.Add('{');
  PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"Width":' + (EdgeWidth) + ',');
  PnPout.Add('"X1":' + (EdgeX1) + ',');
  PnPout.Add('"Y1":' + (EdgeY1) + ',');
  PnPout.Add('"X2":' + (EdgeX2) + ',');
  PnPout.Add('"Y2":' + (EdgeY2));

  PnPout.Add('}');
  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseText(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  Layer: String;
  X1, Y1, X2, Y2, _W, _H: Single;
  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeHeight: String;
  EdgeType: String;
begin
  PnPout := TStringList.Create;

  If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
  Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
  Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer';

  // X1 :=CoordToMMs(Prim.BoundingRectangleNoNameCommentForSignals.Left- Board.XOrigin);
  // Y1 :=CoordToMMs(Prim.BoundingRectangleNoNameCommentForSignals.Bottom- Board.YOrigin);
  // X2 :=CoordToMMs(Prim.BoundingRectangleNoNameCommentForSignals.Right- Board.XOrigin);
  // Y2 :=CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Top- Board.YOrigin);

  // var x0, y0, x1, y1;
  X1 := CoordToMMs(Prim.BoundingRectangleForSelection.Left - Board.XOrigin);
  Y1 := CoordToMMs(Prim.BoundingRectangleForSelection.Bottom - Board.YOrigin);
  X2 := CoordToMMs(Prim.BoundingRectangleForSelection.Right - Board.XOrigin);
  Y2 := CoordToMMs(Prim.BoundingRectangleForSelection.Top - Board.YOrigin);

  _W := X2 - X1;
  _H := Y2 - Y1;

  EdgeX1 := JSONFloatToStr(X1 + _W / 2);
  EdgeY1 := JSONFloatToStr(-(Y1 + _H / 2));

  // bbox["size"] = [(x1 - x0).round(), (y1 - y0).round()];

  // bbox["center"] = [(x0 + bbox.size[0] / 2).round(), -(y0 + bbox.size[1] / 2).round()];

  EdgeX2 := JSONFloatToStr(Prim.Rotation);

  if (Prim.TextKind = 0) then
  begin
    // res["thickness"] = CoordToMMs(Prim.Width).round();
    EdgeHeight := JSONFloatToStr(CoordToMMs(Prim.Size));
    EdgeWidth := EdgeHeight; // single char's width in kicad
  end
  else if (Prim.TextKind = 1) then
  begin
    EdgeHeight := JSONFloatToStr(CoordToMMs(Prim.TTFTextHeight * 0.6));
    EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.TTFTextWidth * 0.9 /
      Length(Prim.Text)));
    // res["thickness"] = CoordToMMs(res["height"] * 0.1);
  end;

  EdgeType := 'text';
  PnPout.Add('{');
  PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  // PnPout.Add('"Width":'+'"'+Preprocess(EdgeWidth)+'"'+',');
  PnPout.Add('"X":' + (EdgeX1) + ',');
  PnPout.Add('"Y":' + (EdgeY1) + ',');
  PnPout.Add('"Width":' + (EdgeWidth) + ',');
  PnPout.Add('"Height":' + (EdgeHeight) + ',');
  PnPout.Add('"Angle":' + (EdgeX2) + ',');
  // PnPout.Add('"Angle2":'+'"'+Preprocess(EdgeY2)+'"'+',');
  PnPout.Add('"Mirrored":' + JSONBoolToStr(Prim.MirrorFlag) + ',');
  PnPout.Add('"Designator":' + JSONBoolToStr(Prim.IsDesignator) + ',');
  PnPout.Add('"Value":' + JSONBoolToStr(Prim.IsComment) + ',');
  PnPout.Add('"Text":' + JSONStrToStr(Prim.Text));
  PnPout.Add('}');
  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseArc2(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  Layer: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeRadius: String;
  EdgeType: String;
begin
  PnPout := TStringList.Create;
  // ;{edges.push(parseArc(Prim));}

  // res["type"] = "arc";
  // res["width"] = width;
  // res["startangle"] = -Prim.EndAngle.round();
  // res["endangle"] = -Prim.StartAngle.round();
  // res["start"] = [CoordToMMs(Prim.XCenter).round(), -CoordToMMs(Prim.YCenter).round()];

  EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.LineWidth));
  EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.XCenter - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.YCenter - Board.YOrigin));
  EdgeX2 := JSONFloatToStr(-Prim.EndAngle);
  EdgeY2 := JSONFloatToStr(-Prim.StartAngle);
  EdgeRadius := JSONFloatToStr(CoordToMMs(Prim.Radius));

  If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
  Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
  Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer';

  EdgeType := 'arc';
  PnPout.Add('{');
  PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"Width":' + (EdgeWidth) + ',');
  PnPout.Add('"X":' + (EdgeX1) + ',');
  PnPout.Add('"Y":' + (EdgeY1) + ',');
  PnPout.Add('"Angle1":' + (EdgeX2) + ',');
  PnPout.Add('"Angle2":' + (EdgeY2) + ',');
  PnPout.Add('"Radius":' + (EdgeRadius));
  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseTrack2(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  Layer: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeHeight: String;
  EdgeType: String;
  Net: String;
begin
  PnPout := TStringList.Create;

  // JSONPush(edges,parseTrack(Prim));
  {
    var start = [CoordToMMs(Prim.x1).round(), -CoordToMMs(Prim.y1).round()];
    var end = [CoordToMMs(Prim.x2).round(), -CoordToMMs(Prim.y2).round()];
    res["layer"] = Prim.Layer;
    if (Prim.InPolygon)
    res["type"] = "polygon";
    res["svgpath"] = ["M", start, "L", end].join(" ");
    else             res["type"] = "segment";
    res["start"] = start;
    res["end"] = end;
    res["width"] = CoordToMMs(Prim.Width).round();
  }

  EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.Width));
  EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.X1 - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.Y1 - Board.YOrigin));
  EdgeX2 := JSONFloatToStr(CoordToMMs(Prim.X2 - Board.XOrigin));
  EdgeY2 := JSONFloatToStr(-CoordToMMs(Prim.Y2 - Board.YOrigin));

  If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
  Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
  Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer';
  Net := 'No Net';
  if Prim.Net <> nil then
    Net := Prim.Net.name;

  EdgeType := 'segment';
  PnPout.Add('{');
  PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"Width":' + (EdgeWidth) + ',');
  PnPout.Add('"X1":' + (EdgeX1) + ',');
  PnPout.Add('"Y1":' + (EdgeY1) + ',');
  PnPout.Add('"X2":' + (EdgeX2) + ',');
  PnPout.Add('"Y2":' + (EdgeY2) + ',');
  PnPout.Add('"Net":' + JSONStrToStr(Net));
  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseVIA(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  Layer, Net: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  EdgeWidth, EdgeX1, EdgeY1, PadDrillWidth: String;
  EdgeType: String;
begin
  PnPout := TStringList.Create;

  // TODO: Layers
  EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.Size));
  EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.x - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.Y - Board.YOrigin));
  PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleSize));

  If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
  Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
  Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer'
  Else If (Prim.Layer = eMultiLayer) Then
    Layer := 'MultiLayer';
  Net := 'No Net';
  if Prim.Net <> nil then
    Net := Prim.Net.name;

  EdgeType := 'via';
  PnPout.Add('{');
  PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"Width":' + JSONStrToStr(EdgeWidth) + ',');
  PnPout.Add('"X":' + (EdgeX1) + ',');
  PnPout.Add('"Y":' + (EdgeY1) + ',');
  PnPout.Add('"DrillWidth":' + (PadDrillWidth) + ',');
  PnPout.Add('"Net":' + JSONStrToStr(Net));
  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseRegion(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  Layer, Net: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  // EdgeWidth, EdgeX1, EdgeY1, PadDrillWidth: String;
  EdgeX1, EdgeY1: String;
  EdgeType: String;
  k: Integer;
  CI1, CO1: TObject;
  contour: IPCB_Contour;
  kk: Integer;
begin
  PnPout := TStringList.Create;

  If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
  Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
  Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer';
  Net := 'No Net';
  if Prim.Net <> nil then
    Net := Prim.Net.name;

  EdgeType := 'polygon';
  PnPout.Add('{');
  PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"Net":' + JSONStrToStr(Net) + ',');
  PnPout.Add('"Points": [');
  PnPout.Add('],');
  // PnPout.Add('}');

  k := 0;

  PnPout.Add('"EX": [');

  // CI1 := Prim.GroupIterator_Create;
  // CI1.AddFilter_ObjectSet(MkSet(ePadObject));
  // CO1 := CI1.FirstPCBObject;
  // While (CO1 <> Nil) Do
  CO1 := Prim;

  // if (CO1<>nil) then
  begin
    if CO1.ObjectId = eRegionObject then
    begin
      Inc(k);
      If (k > 1) Then
        PnPout.Add(',');
      PnPout.Add('[');
      contour := CO1.GetMainContour();
      for kk := 0 to contour.Count do
      begin
        If (kk > 0) Then
          PnPout.Add(',');
        EdgeX1 := JSONFloatToStr
          (CoordToMMs(contour.GetState_PointX(kk mod contour.Count) -
          Board.XOrigin));
        EdgeY1 := JSONFloatToStr
          (-CoordToMMs(contour.GetState_PointY(kk mod contour.Count) -
          Board.YOrigin));
        PnPout.Add('[');
        PnPout.Add(EdgeX1 + ',');
        PnPout.Add(EdgeY1);
        PnPout.Add(']');
      end;
      PnPout.Add(']');
    end;
    // CO1 := CI1.NextPCBObject;
  end;
  // Prim.GroupIterator_Destroy(CI1);

  PnPout.Add(']');

  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParsePoly(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  Layer, Net: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  // EdgeWidth, EdgeX1, EdgeY1, PadDrillWidth: String;
  EdgeX1, EdgeY1: String;
  EdgeType: String;
  k: Integer;
  CI1, CO1: TObject;
  contour: IPCB_Contour;
  kk: Integer;
begin
  PnPout := TStringList.Create;

  If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
  Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
  Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer';
  Net := 'No Net';
  if Prim.Net <> nil then
    Net := Prim.Net.name;

  EdgeType := 'polygon';
  PnPout.Add('{');
  PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"Net":' + JSONStrToStr(Net) + ',');
  PnPout.Add('"Points": [');
  PnPout.Add('],');
  // PnPout.Add('}');

  k := 0;

  PnPout.Add('"EX": [');

  CI1 := Prim.GroupIterator_Create;
  // CI1.AddFilter_ObjectSet(MkSet(ePadObject));
  CO1 := CI1.FirstPCBObject;
  While (CO1 <> Nil) Do
  begin
    if CO1.ObjectId = eRegionObject then
    begin
      Inc(k);
      If (k > 1) Then
        PnPout.Add(',');
      PnPout.Add('[');
      contour := CO1.GetMainContour();
      for kk := 0 to contour.Count do
      begin
        If (kk > 0) Then
          PnPout.Add(',');
        EdgeX1 := JSONFloatToStr
          (CoordToMMs(contour.GetState_PointX(kk mod contour.Count) -
          Board.XOrigin));
        EdgeY1 := JSONFloatToStr
          (-CoordToMMs(contour.GetState_PointY(kk mod contour.Count) -
          Board.YOrigin));
        PnPout.Add('[');
        PnPout.Add(EdgeX1 + ',');
        PnPout.Add(EdgeY1);
        PnPout.Add(']');
      end;
      PnPout.Add(']');
    end;
    CO1 := CI1.NextPCBObject;
  end;
  Prim.GroupIterator_Destroy(CI1);

  PnPout.Add(']');

  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParsePad(Board: IPCB_Board; Prim, Pad: TObject): String;
var
  PnPout: TStringList;
  // Layer, Net: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  // EdgeWidth, EdgeX1, EdgeY1, PadDrillWidth: String;
  // EdgeType: String;
  PadsCount: Integer;
  PadLayer, PadType: String;
  PadX, PadY, PadAngle: TString;
  X1, Y1, X2, Y2, _W, _H: Single;
  Width, Height: String;
  PadWidth, PadHeight: String;
  PadPin1: Boolean;
  PadShape, PadDrillShape: String;
  PadDrillWidth, PadDrillHeight: String;
  Net: String;
begin
  PnPout := TStringList.Create;

  PnPout.Add('{');

  // TODO: Not sure
  PadType := Pad.Layer;
  if (Pad.Layer = eTopLayer) then
  begin
    PadLayer := 'TopLayer';
    PadType := 'smd';
    PadWidth := FloatToStr(CoordToMMs(Pad.TopXSize));
    PadHeight := FloatToStr(CoordToMMs(Pad.TopYSize));

    PadShape := 'circle';
    case (Pad.TopShape) of
      1:
        begin
          if PadWidth = PadHeight then
            PadShape := 'circle'
          else
            PadShape := 'oval';
          // (res['size'][0] == res['size'][1]) ? 'circle' : 'oval';
        end;
      2:
        PadShape := 'rect';
      // 3:PadShape :='chamfrect';
      9:
        PadShape := 'roundrect';
      // default:
      // res['shape'] :='custom';
    end;
  end
  else if (Pad.Layer = eBottomLayer) then
  begin
    PadLayer := 'BottomLayer';
    PadType := 'smd';
    PadWidth := FloatToStr(CoordToMMs(Prim.BotXSize));
    PadHeight := FloatToStr(CoordToMMs(Prim.BotYSize));
    PadShape := 'circle';
    case (Pad.BotShape) of
      1:
        begin
          if PadWidth = PadHeight then
            PadShape := 'circle'
          else
            PadShape := 'oval';
          // (res['size'][0] == res['size'][1]) ? 'circle' : 'oval';
        end;
      2:
        PadShape := 'rect';
      // 3:PadShape :='chamfrect';
      9:
        PadShape := 'roundrect';
      // default:
      // res['shape'] :='custom';
    end;
  end
  else
  begin
    PadLayer := 'MultiLayer';
    PadType := 'th';
    PadWidth := FloatToStr(CoordToMMs(Prim.TopXSize));
    PadHeight := FloatToStr(CoordToMMs(Prim.TopYSize));
    // TODO: Is it norm?
    PadShape := 'circle';
    case (Pad.TopShape) of
      1:
        begin
          if PadWidth = PadHeight then
            PadShape := 'circle'
          else
            PadShape := 'oval';
          // (res['size'][0] == res['size'][1]) ? 'circle' : 'oval';
        end;
      // 3:PadShape :='chamfrect';
      9:
        PadShape := 'roundrect';
      // default:
      // res['shape'] :='custom';
    end;
    case (Pad.BotShape) of
      1:
        begin
          if PadWidth = PadHeight then
            PadShape := 'circle'
          else
            PadShape := 'oval';
          // (res['size'][0] == res['size'][1]) ? 'circle' : 'oval';
        end;
      2:
        PadShape := 'rect';
      // 3:PadShape :='chamfrect';
      9:
        PadShape := 'roundrect';
      // default:
      // res['shape'] :='custom';
    end;

    case (Pad.HoleType) of
      0: // circle
        begin
          // res["drillsize"] = [CoordToMMs(Prim.HoleSize).round(), CoordToMMs(Prim.HoleSize).round()];
          PadDrillShape := 'circle';
          PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleSize));
          PadDrillHeight := JSONFloatToStr(CoordToMMs(Prim.HoleSize));
        end;
      1: // square, but not supported in kicad, so do as circle
        begin
          // res["drillsize"] = [CoordToMMs(Prim.HoleSize).round(), CoordToMMs(Prim.HoleSize).round()];
          PadDrillShape := 'rect';
          PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleWidth));
          PadDrillHeight := JSONFloatToStr(CoordToMMs(Prim.HoleSize));
        end;
      2: // slot
        begin
          // res["drillsize"] = [CoordToMMs(Prim.HoleWidth).round(), CoordToMMs(Prim.HoleSize).round()];
          PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleWidth));
          PadDrillHeight := JSONFloatToStr(CoordToMMs(Prim.HoleSize));
          PadDrillShape := 'oblong';
        end;
      // default:  //
    end;

  end;

  PadPin1 := False;
  if (Pad.name = '1') then
  // if ("A1".indexOf(Pad.Name) != -1) then
  begin
    PadPin1 := True;
  end;

  PadWidth := StringReplace(PadWidth, ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));
  PadHeight := StringReplace(PadHeight, ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));

  PadX := JSONFloatToStr(CoordToMMs(Pad.x - Board.XOrigin));
  PadY := JSONFloatToStr(-CoordToMMs(Pad.Y - Board.YOrigin));

  PadAngle := JSONFloatToStr(Pad.Rotation);

  Net := 'No Net';
  if Prim.Net <> nil then
    Net := Prim.Net.name;

  PnPout.Add('"Layer":' + JSONStrToStr(PadLayer) + ',');
  PnPout.Add('"Type":' + JSONStrToStr(PadType) + ',');
  PnPout.Add('"Shape":' + JSONStrToStr(PadShape) + ',');
  if PadType = 'th' then
  begin
    PnPout.Add('"DrillShape":' + JSONStrToStr(PadDrillShape) + ',');
    PnPout.Add('"DrillWidth":' + (PadDrillWidth) + ',');
    PnPout.Add('"DrillHeight":' + (PadDrillHeight) + ',');
  end;
  // PnPout.Add('"Debug":'+'"'+Preprocess(Prim.Layer)+'"'+',');
  PnPout.Add('"X":' + (PadX) + ',');
  PnPout.Add('"Y":' + (PadY) + ',');
  PnPout.Add('"Width":' + (PadWidth) + ',');
  PnPout.Add('"Height":' + (PadHeight) + ',');
  PnPout.Add('"Angle":' + (PadAngle) + ',');
  PnPout.Add('"Pin1":' + JSONBoolToStr(PadPin1) + ',');
  PnPout.Add('"Net":' + JSONStrToStr(Net));

  PnPout.Add('}');
  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseComponent(Board: IPCB_Board; Component: TObject;
  SelectedFields, SelectedGroupParameters: TStringList; NoBOM: Boolean): String;
var
  PnPout: TStringList;
  Iterator: IPCB_BoardIterator;
  ComponentIterator: IPCB_GroupIterator;

  Pad: IPCB_Pad;

  x, Y, Rotation, Layer, Net: TString;

  Iter, Prim: TObject;
  PadsCount: Integer;
  X1, Y1, X2, Y2, _W, _H: Single;
  Width, Height: String;

  sl: TStringList;
  hhhhi: Integer;
  hhhh: String;
begin
  PnPout := TStringList.Create;

  If (Component.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else
    Layer := 'BottomLayer';
  x := JSONFloatToStr(CoordToMMs(Component.x - Board.XOrigin));
  Y := JSONFloatToStr(-CoordToMMs(Component.Y - Board.YOrigin));
  Rotation := IntToStr(Component.Rotation);

  // TODO: Is it correct? X1,Y1 vs X,Y
  X1 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Left -
    Board.XOrigin);
  Y1 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Bottom -
    Board.YOrigin);
  X2 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Right -
    Board.XOrigin);
  Y2 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Top -
    Board.YOrigin);

  Width := JSONFloatToStr(X2 - X1);
  Height := JSONFloatToStr(Y2 - Y1);

  x := StringReplace(FloatToStr(X1), ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));
  Y := StringReplace(FloatToStr(-Y2), ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));

  {
    bbox['pos'] :=[x0.round(), -y1.round()]; //
    bbox['relpos'] :=[0, 0];
    bbox['angle'] :=0;
    bbox['size'] :=[(x1 - x0).round(), (y1 - y0).round()];
    bbox['center'] :=[(x0 + bbox.size[0] / 2).round(), -(y0 + bbox.size[1] / 2).round()];
  }

  PnPout.Add('{');

  PnPout.Add('"Designator":' + JSONStrToStr(Component.SourceDesignator) + ',');
  PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"Footprint":' + JSONStrToStr(Component.Pattern) + ',');

  sl := GetComponentParameters(Component);

  PnPout.Add('"PartNumber":' + JSONStrToStr
    (sl.Values[ValueParameterName]) + ','); // TODO: CopyPaste error?
  PnPout.Add('"Value":' + JSONStrToStr(sl.Values[ValueParameterName]) + ',');

  PnPout.Add('"Fields":' + '[');

  for hhhhi := 0 to SelectedFields.Count - 1 do
  begin
    if (hhhhi > 0) then
      PnPout.Add(',');
    hhhh := SelectedFields[hhhhi];
    PnPout.Add(JSONStrToStr(sl.Values[hhhh]));
  end;
  PnPout.Add('],');

  PnPout.Add('"Group":' + '[');

  for hhhhi := 0 to SelectedGroupParameters.Count - 1 do
  begin
    if (hhhhi > 0) then
      PnPout.Add(',');
    hhhh := SelectedGroupParameters[hhhhi];
    PnPout.Add(JSONStrToStr(sl.Values[hhhh]));
  end;
  PnPout.Add('],');

  PnPout.Add('"X":' + (x) + ',');
  PnPout.Add('"Y":' + (Y) + ',');
  PnPout.Add('"Width":' + (Width) + ',');
  PnPout.Add('"Height":' + (Height) + ',');
  PnPout.Add('"NoBOM":' + JSONBoolToStr(NoBOM) + ',');

  PnPout.Add('"Pads":' + '[');

  PadsCount := 0;
  Iter := Component.GroupIterator_Create;
  Iter.AddFilter_ObjectSet(MkSet(ePadObject));
  Iter.AddFilter_LayerSet(AllLayers);
  Prim := Iter.FirstPCBObject;
  while (Prim <> nil) do
  begin
    Pad := Prim;
    Inc(PadsCount);
    If (PadsCount > 1) Then
      PnPout.Add(',');

    PnPout.Add(ParsePad(Board, Prim, Pad));

    // pads :=pads.concat(parsePad(Prim));
    // if (isSMD and (Prim.Layer = eMultiLayer)) then begin
    // isSMD :=false;
    // end;
    Prim := Iter.NextPCBObject;
  end;
  Component.GroupIterator_Destroy(Iter);

  PnPout.Add(']');

  PnPout.Add('}');
  Result := PnPout.Text;
  PnPout.Free;
end;

function PickAndPlaceOutputNative(OutputJS: Boolean): String;
var
  Board: IPCB_Board; // document board object
  Component: IPCB_Component; // component object
  Iterator: IPCB_BoardIterator;
  SMDcomponent: Boolean;
  BoardUnits: String;
  // Current unit string mm/mils
  PnPout: TStringList;
  Count: Integer;
  FileName: TString;
  Document: IServerDocument;
  pcbDocPath: TString;
  flagRequirePcbDocFile: Boolean;
  Separator: TString;
  Iter, Prim: TObject;
  PadsCount: Integer;
  X1, Y1, X2, Y2, _W, _H: Single;
  Width, Height: String;
  CurrParm: IParameter;
  NoBOM: Boolean;
  ccc: IComponent;

  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeRadius: String;

  _Document: IServerDocument;
  // sl: TStringList;
  tmpx, tmpy: String;
  SelectedFields: TStringList;
  SelectedGroupParameters: TStringList;
  hhhhi: Integer;
  hhhh: String;
  StartTime, StopTime: TDateTime;
  Elapsed: Integer;
Begin
  // Make sure the current Workspace opens or else quit this script
  CurrWorkSpace := GetWorkSpace;
  If (CurrWorkSpace = Nil) Then
    Exit;

  // Make sure the currently focussed Project in this Workspace opens or else
  // quit this script
  CurrProject := CurrWorkSpace.DM_FocusedProject;
  If CurrProject = Nil Then
    Exit;

  flagRequirePcbDocFile := True;

  FindProjectPcbDocFile(CurrProject, flagRequirePcbDocFile,
    { var } pcbDocPath);

  // TODO: Close
  _Document := Client.OpenDocument('pcb', pcbDocPath);
  Board := PCBServer.GetPCBBoardByPath(pcbDocPath);

  If Not Assigned(Board) Then // check of active document
  Begin
    ShowMessage('The Current Document is not a PCB Document.');
    Exit;
  End;

  StartTime := Now();

  SelectedFields := GetSelectedFields();
  SelectedGroupParameters := GetSelectedGroupParameters();

  Iterator := Board.BoardIterator_Create;
  Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
  Iterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
  Iterator.AddFilter_Method(eProcessAll);

  Separator := GetSeparator();
  Count := 0;
  PnPout := TStringList.Create;
  Component := Iterator.FirstPCBObject;

  if (OutputJS) then
  begin
    PnPout.Add('var altiumbom = ');
  end;
  PnPout.Add('{');
  PnPout.Add('"Signature":' + JSONStrToStr('Altium') + ',');
  PnPout.Add('"Data":[');

  While (Component <> Nil) Do
  Begin
    NoBOM := False;
    ccc := GetCompFromCompEx(Component);
    CurrParm := ccc.DM_GetParameterByName('Component Kind');
    // (CurrParm <> nil) and
    if (CurrParm.DM_Value = 'Standard (No BOM)') then
    begin
      NoBOM := True;
    end;

    // Print Pick&Place data of SMD components to file
    if ComponentIsFittedInCurrentVariant(Component.SourceUniqueId, Component.SourceDesignator,
      ProjectVariant) then
      if (LayerFilterIndex = 0) or
        ((LayerFilterCb = 1) and (Component.Layer = eTopLayer)) or
        ((LayerFilterCb = 2) and (Component.Layer = eBottomLayer)) then
      Begin
        Inc(Count);
        If (Count > 1) Then
          PnPout.Add(',');

        PnPout.Add(ParseComponent(Board, Component, SelectedFields, SelectedGroupParameters, NoBOM));
      End;
    Component := Iterator.NextPCBObject;
  End;
  Board.BoardIterator_Destroy(Iterator);

  PnPout.Add('],');
  PnPout.Add('"Board":[');

  Count := 0;

  Iter := Board.BoardIterator_Create;
  // Iter.AddFilter_ObjectSet(eTrackObject);
  Iter.AddFilter_LayerSet(MkSet(eKeepOutLayer));
  Iter.AddFilter_ObjectSet(MkSet(eArcObject, eTrackObject));
  // Iter.AddFilter_ObjectSet(MkSet(eTrackObject));
  { Iter.AddFilter_LayerSet(MkSet(pcb.Layers.OUTLINE_LAYER)); }
  Iter.AddFilter_Method(eProcessAll);
  Prim := Iter.FirstPCBObject;
  while (Prim <> nil) do
  begin
    Inc(Count);
    If (Count > 1) Then
      PnPout.Add(',');

    case (Prim.ObjectId) of
      eArcObject:
        begin
          PnPout.Add(ParseArc(Board, Prim));
        end;
      eTrackObject:
        begin
          PnPout.Add(ParseTrack(Board, Prim));
        end;
    end;

    Prim := Iter.NextPCBObject;
  end;
  Board.BoardIterator_Destroy(Iter);

  PnPout.Add('],');
  PnPout.Add('"BB":{');

  // var bbox = {};
  // bbox["minx"] = CoordToMMs(Board.BoardOutline.BoundingRectangle.Left).round();
  // bbox["miny"] = -CoordToMMs(pcb.board.BoardOutline.BoundingRectangle.Top).round();
  // bbox["maxx"] = CoordToMMs(pcb.board.BoardOutline.BoundingRectangle.Right).round();
  // bbox["maxy"] = -CoordToMMs(pcb.board.BoardOutline.BoundingRectangle.Bottom).round();

  EdgeX1 := JSONFloatToStr(CoordToMMs(Board.BoardOutline.BoundingRectangle.Left
    - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Board.BoardOutline.BoundingRectangle.Top
    - Board.YOrigin));
  EdgeX2 := JSONFloatToStr(CoordToMMs(Board.BoardOutline.BoundingRectangle.Right
    - Board.XOrigin));
  EdgeY2 := JSONFloatToStr
    (-CoordToMMs(Board.BoardOutline.BoundingRectangle.Bottom - Board.YOrigin));

  PnPout.Add('"X1":' + (EdgeX1) + ',');
  PnPout.Add('"Y1":' + (EdgeY1) + ',');
  PnPout.Add('"X2":' + (EdgeX2) + ',');
  PnPout.Add('"Y2":' + (EdgeY2));

  PnPout.Add('},');
  PnPout.Add('"Extra":[');

  Count := 0;

  Iter := Board.BoardIterator_Create;
  // Iter.AddFilter_ObjectSet(eTrackObject);
  Iter.AddFilter_LayerSet(MkSet(eTopOverlay, eBottomOverlay, eTopLayer,
    eBottomLayer, eMultiLayer));
  Iter.AddFilter_ObjectSet(MkSet(eArcObject, eTrackObject, eTextObject,
    ePolyObject, eRegionObject, eViaObject));
  // Iter.AddFilter_ObjectSet(MkSet(eArcObject, eTrackObject, eTextObject ));
  // eFillObject, eRegionObject
  // Iter.AddFilter_ObjectSet(MkSet(eViaObject));
  // Iter.AddFilter_ObjectSet(MkSet(eTrackObject));
  { Iter.AddFilter_LayerSet(MkSet(pcb.Layers.OUTLINE_LAYER)); }
  Iter.AddFilter_Method(eProcessAll);
  Prim := Iter.FirstPCBObject;
  while (Prim <> nil) do
  begin
    // TODO: UGLY
    if (Prim.ObjectId = eTextObject) and (Prim.IsHidden) then
    begin
      Prim := Iter.NextPCBObject;
      continue;
    end;
    if ((Prim.Layer = eTopLayer) or (Prim.Layer = eBottomLayer)) and
      (Prim.ObjectId <> eTrackObject) and (Prim.ObjectId <> eViaObject) and
      (Prim.ObjectId <> ePolyObject) and (Prim.ObjectId <> eFillObject) and
      (Prim.ObjectId <> eRegionObject) then
    begin
      Prim := Iter.NextPCBObject;
      continue;
    end;
    if (Prim.ObjectId = eRegionObject) and
      ((Prim.Kind() <> eRegionKind_Copper) or Prim.InPolygon()) then
    begin
      Prim := Iter.NextPCBObject;
      continue;
    end;
    if ((Prim.ObjectId = ePolyObject)) and
      ((Prim.Layer <> eTopLayer) and (Prim.Layer <> eBottomLayer)) then
    begin
      Prim := Iter.NextPCBObject;
      continue;
    end;

    Inc(Count);
    If (Count > 1) Then
      PnPout.Add(',');

    case (Prim.ObjectId) of
      eTextObject:
        begin
          PnPout.Add(ParseText(Board, Prim));
        end;
      eArcObject:
        begin
          PnPout.Add(ParseArc2(Board, Prim));
        end;
      eTrackObject:
        begin
          PnPout.Add(ParseTrack2(Board, Prim));
        end;
      eViaObject:
        begin
          PnPout.Add(ParseVIA(Board, Prim));
        end;
      eRegionObject:
        begin
          PnPout.Add(ParseRegion(Board, Prim));
        end;
      ePolyObject:
        begin
          PnPout.Add(ParsePoly(Board, Prim));
        end;
    end;

    Prim := Iter.NextPCBObject;
  end;
  Board.BoardIterator_Destroy(Iter);

  PnPout.Add('],');
  PnPout.Add('"Metadata":{');

  PnPout.Add('"Company":' + JSONStrToStr(Company) + ',');
  PnPout.Add('"Date":' + JSONStrToStr('todo2') + ',');
  PnPout.Add('"Revision":' + JSONStrToStr(Revision) + ',');
  PnPout.Add('"Title":' + JSONStrToStr(Title));

  PnPout.Add('},');
  PnPout.Add('"Settings":{');
  PnPout.Add('"AddNets":' + JSONBoolToStr(AddNets) + ',');
  PnPout.Add('"AddTracks":' + JSONBoolToStr(AddTracks) + ',');

  PnPout.Add('"Fields":' + '[');

  for hhhhi := 0 to SelectedFields.Count - 1 do
  begin
    if (hhhhi > 0) then
      PnPout.Add(',');
    hhhh := SelectedFields[hhhhi];
    PnPout.Add(JSONStrToStr(hhhh));
  end;
  PnPout.Add(']');

  PnPout.Add('}');
  PnPout.Add('}');

  if (OutputJS) then
  begin
    PnPout.Add(';');
  end;

  Result := PnPout.Text;

  PnPout.Free;

  {
    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
    Client.ShowDocument(Document);

    ShowMessage(IntToStr(Count) + ' were exported to Pick and Place file:' + #13 + Board.FileName + '.pic');
  }
  StopTime := Now();
  Elapsed := Trunc((StopTime - StartTime) * 86400 * 1000);
  // ShowMessage('Script execution complete in ' + IntToStr(Elapsed) + 'ms');
End;


{.....................................................................................................................}
{.                                              Generic JSON Generation                                              .}
{.....................................................................................................................}


function ParseArcGeneric(Board: IPCB_Board; Prim: TObject): String;
var
  PnPout: TStringList;
  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeRadius: String;
  EdgeType: String;
begin
  PnPout := TStringList.Create;

  EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.LineWidth));
  EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.XCenter - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.YCenter - Board.YOrigin));
  EdgeX2 := JSONFloatToStr(-Prim.EndAngle);
  EdgeY2 := JSONFloatToStr(-Prim.StartAngle);
  EdgeRadius := JSONFloatToStr(CoordToMMs(Prim.Radius));

  EdgeType := 'arc';

  PnPout.Add('{');
  PnPout.Add('"start":' + '[' + EdgeX1 + ', ' + EdgeY1 + ']' + ',');
  PnPout.Add('"endangle":' + EdgeY2 + ',');
  PnPout.Add('"width":' + EdgeWidth + ',');
  PnPout.Add('"radius":' + EdgeRadius + ',');
  PnPout.Add('"startangle":' + EdgeX2 + ',');
  PnPout.Add('"type":' + JSONStrToStr(EdgeType));
  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseTrackGeneric(Board: IPCB_Board; Prim: TObject;
  NoType: Boolean): String;
var
  PnPout: TStringList;
  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeRadius: String;
  EdgeType: String;
begin
  PnPout := TStringList.Create;

  EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.Width));
  EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.X1 - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.Y1 - Board.YOrigin));
  EdgeX2 := JSONFloatToStr(CoordToMMs(Prim.X2 - Board.XOrigin));
  EdgeY2 := JSONFloatToStr(-CoordToMMs(Prim.Y2 - Board.YOrigin));

  EdgeType := 'segment';

  PnPout.Add('{');
  if not NoType then
    PnPout.Add('"type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"start":' + '[' + EdgeX1 + ', ' + EdgeY1 + ']' + ',');
  PnPout.Add('"end":' + '[' + EdgeX2 + ', ' + EdgeY2 + ']' + ',');
  PnPout.Add('"width":' + EdgeWidth);
  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseVIAGeneric(Board: IPCB_Board; Prim: TObject;
  NoType: Boolean): String;
var
  PnPout: TStringList;
  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeRadius: String;
  EdgeType: String;
  PadDrillWidth: String;
begin
  PnPout := TStringList.Create;

  EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.Size));
  EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.x - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.Y - Board.YOrigin));
  PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleSize));

  EdgeType := 'segment';

  PnPout.Add('{');
  if not NoType then
    PnPout.Add('"type":' + JSONStrToStr(EdgeType) + ',');
  PnPout.Add('"start":' + '[' + EdgeX1 + ', ' + EdgeY1 + ']' + ',');
  PnPout.Add('"end":' + '[' + EdgeX1 + ', ' + EdgeY1 + ']' + ',');
  PnPout.Add('"width":' + EdgeWidth + ',');
  PnPout.Add('"drillsize":' + PadDrillWidth);
  PnPout.Add('}');

  {
    // TODO: Layers
    EdgeWidth := JSONFloatToStr(CoordToMMs(Prim.Size));
    EdgeX1 := JSONFloatToStr(CoordToMMs(Prim.x - Board.XOrigin));
    EdgeY1 := JSONFloatToStr(-CoordToMMs(Prim.Y - Board.YOrigin));
    PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleSize));

    If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
    Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
    Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
    Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer'
    Else If (Prim.Layer = eMultiLayer) Then
    Layer := 'MultiLayer';
    Net := 'No Net';
    if Prim.Net <> nil then
    Net := Prim.Net.name;

    PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
    PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
    PnPout.Add('"Width":' + JSONStrToStr(EdgeWidth) + ',');
    PnPout.Add('"X":' + (EdgeX1) + ',');
    PnPout.Add('"Y":' + (EdgeY1) + ',');
    PnPout.Add('"DrillWidth":' + (PadDrillWidth) + ',');
    PnPout.Add('"Net":' + JSONStrToStr(Net));
  }

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseComponentGeneric(Board: IPCB_Board; Component: TObject;
  SelectedFields, SelectedGroupParameters: TStringList; NoBOM: Boolean): String;
var
  PnPout: TStringList;
  Iterator: IPCB_BoardIterator;
  ComponentIterator: IPCB_GroupIterator;

  Pad: IPCB_Pad;

  x, Y, Rotation, Layer, Net: TString;

  Iter, Prim: TObject;
  PadsCount: Integer;
  X1, Y1, X2, Y2, _W, _H: Single;
  Width, Height: String;

  sl: TStringList;
  hhhhi: Integer;
  hhhh: String;
begin
  PnPout := TStringList.Create;

  If (Component.Layer = eTopLayer) Then
    Layer := 'F'
  Else
    Layer := 'B';
  x := JSONFloatToStr(CoordToMMs(Component.x - Board.XOrigin));
  Y := JSONFloatToStr(-CoordToMMs(Component.Y - Board.YOrigin));
  Rotation := IntToStr(Component.Rotation);

  // TODO: Is it correct? X1,Y1 vs X,Y
  X1 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Left -
    Board.XOrigin);
  Y1 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Bottom -
    Board.YOrigin);
  X2 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Right -
    Board.XOrigin);
  Y2 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Top -
    Board.YOrigin);

  Width := JSONFloatToStr(X2 - X1);
  Height := JSONFloatToStr(Y2 - Y1);

  x := StringReplace(FloatToStr(X1), ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));
  Y := StringReplace(FloatToStr(-Y2), ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));

  {
    bbox['pos'] :=[x0.round(), -y1.round()]; //
    bbox['relpos'] :=[0, 0];
    bbox['angle'] :=0;
    bbox['size'] :=[(x1 - x0).round(), (y1 - y0).round()];
    bbox['center'] :=[(x0 + bbox.size[0] / 2).round(), -(y0 + bbox.size[1] / 2).round()];
  }

  PnPout.Add('{');
  PnPout.Add('"attr":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"footprint":' + JSONStrToStr(Component.Pattern) + ',');
  PnPout.Add('"layer":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"ref":' + JSONStrToStr(Component.SourceDesignator) + ',');
  PnPout.Add('"val":' + JSONStrToStr(Layer) + ',');
  PnPout.Add('"extra_fields":' + '{}');

  // attr? extra_fields?

  (*

    sl := GetComponentParameters(Component);

    PnPout.Add('"PartNumber":' + JSONStrToStr
    (sl.Values[ValueParameterName]) + ',');
    PnPout.Add('"Value":' + JSONStrToStr(sl.Values[ValueParameterName]) + ',');

    PnPout.Add('"Fields":' + '[');

    for hhhhi := 0 to SelectedFields.Count - 1 do
    begin
    if (hhhhi > 0) then
    PnPout.Add(',');
    hhhh := SelectedFields[hhhhi];
    PnPout.Add(JSONStrToStr(sl.Values[hhhh]));
    end;
    PnPout.Add('],');

    PnPout.Add('"Group":' + '[');

    for hhhhi := 0 to SelectedGroupParameters.Count - 1 do
    begin
    if (hhhhi > 0) then
    PnPout.Add(',');
    hhhh := SelectedGroupParameters[hhhhi];
    PnPout.Add(JSONStrToStr(sl.Values[hhhh]));
    end;
    PnPout.Add('],');

    PnPout.Add('"NoBOM":' + JSONBoolToStr(NoBOM) + ',');

  *)
  PnPout.Add('}');
  Result := PnPout.Text;
  PnPout.Free;
end;

function ParsePadGeneric(Board: IPCB_Board; Prim, Pad: TObject): String;
var
  PnPout: TStringList;
  // Layer, Net: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  // EdgeWidth, EdgeX1, EdgeY1, PadDrillWidth: String;
  // EdgeType: String;
  PadsCount: Integer;
  PadLayer, PadType: String;
  PadX, PadY, PadAngle: TString;
  X1, Y1, X2, Y2, _W, _H: Single;
  Width, Height: String;
  PadWidth, PadHeight: String;
  PadPin1: Boolean;
  PadShape, PadDrillShape: String;
  PadDrillWidth, PadDrillHeight: String;
  Net: String;
begin
  PnPout := TStringList.Create;

  PnPout.Add('{');

  // TODO: Not sure
  PadType := Pad.Layer;
  if (Pad.Layer = eTopLayer) then
  begin
    PadLayer := '["F"]';
    PadType := 'smd';
    PadWidth := FloatToStr(CoordToMMs(Pad.TopXSize));
    PadHeight := FloatToStr(CoordToMMs(Pad.TopYSize));

    PadShape := 'circle';
    case (Pad.TopShape) of
      1:
        begin
          if PadWidth = PadHeight then
            PadShape := 'circle'
          else
            PadShape := 'oval';
          // (res['size'][0] == res['size'][1]) ? 'circle' : 'oval';
        end;
      2:
        PadShape := 'rect';
      // 3:PadShape :='chamfrect';
      9:
        PadShape := 'roundrect';
      // default:
      // res['shape'] :='custom';
    end;
  end
  else if (Pad.Layer = eBottomLayer) then
  begin
    PadLayer := '["B"]';
    PadType := 'smd';
    PadWidth := FloatToStr(CoordToMMs(Prim.BotXSize));
    PadHeight := FloatToStr(CoordToMMs(Prim.BotYSize));
    PadShape := 'circle';
    case (Pad.BotShape) of
      1:
        begin
          if PadWidth = PadHeight then
            PadShape := 'circle'
          else
            PadShape := 'oval';
          // (res['size'][0] == res['size'][1]) ? 'circle' : 'oval';
        end;
      2:
        PadShape := 'rect';
      // 3:PadShape :='chamfrect';
      9:
        PadShape := 'roundrect';
      // default:
      // res['shape'] :='custom';
    end;
  end
  else
  begin
    PadLayer := '["F", "B"]';
    PadType := 'th';
    PadWidth := FloatToStr(CoordToMMs(Prim.TopXSize));
    PadHeight := FloatToStr(CoordToMMs(Prim.TopYSize));
    // TODO: Is it norm?
    PadShape := 'circle';
    case (Pad.TopShape) of
      1:
        begin
          if PadWidth = PadHeight then
            PadShape := 'circle'
          else
            PadShape := 'oval';
          // (res['size'][0] == res['size'][1]) ? 'circle' : 'oval';
        end;
      // 3:PadShape :='chamfrect';
      9:
        PadShape := 'roundrect';
      // default:
      // res['shape'] :='custom';
    end;
    case (Pad.BotShape) of
      1:
        begin
          if PadWidth = PadHeight then
            PadShape := 'circle'
          else
            PadShape := 'oval';
          // (res['size'][0] == res['size'][1]) ? 'circle' : 'oval';
        end;
      2:
        PadShape := 'rect';
      // 3:PadShape :='chamfrect';
      9:
        PadShape := 'roundrect';
      // default:
      // res['shape'] :='custom';
    end;

    case (Pad.HoleType) of
      0: // circle
        begin
          // res["drillsize"] = [CoordToMMs(Prim.HoleSize).round(), CoordToMMs(Prim.HoleSize).round()];
          PadDrillShape := 'circle';
          PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleSize));
          PadDrillHeight := JSONFloatToStr(CoordToMMs(Prim.HoleSize));
        end;
      1: // square, but not supported in kicad, so do as circle
        begin
          // res["drillsize"] = [CoordToMMs(Prim.HoleSize).round(), CoordToMMs(Prim.HoleSize).round()];
          PadDrillShape := 'rect';
          PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleWidth));
          PadDrillHeight := JSONFloatToStr(CoordToMMs(Prim.HoleSize));
        end;
      2: // slot
        begin
          // res["drillsize"] = [CoordToMMs(Prim.HoleWidth).round(), CoordToMMs(Prim.HoleSize).round()];
          PadDrillWidth := JSONFloatToStr(CoordToMMs(Prim.HoleWidth));
          PadDrillHeight := JSONFloatToStr(CoordToMMs(Prim.HoleSize));
          PadDrillShape := 'oblong';
        end;
      // default:  //
    end;

  end;

  PadPin1 := False;
  if (Pad.name = '1') then
  // if ("A1".indexOf(Pad.Name) != -1) then
  begin
    PadPin1 := True;
  end;

  PadWidth := StringReplace(PadWidth, ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));
  PadHeight := StringReplace(PadHeight, ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));

  PadX := JSONFloatToStr(CoordToMMs(Pad.x - Board.XOrigin));
  PadY := JSONFloatToStr(-CoordToMMs(Pad.Y - Board.YOrigin));

  PadAngle := JSONFloatToStr(Pad.Rotation);

  Net := 'No Net';
  if Prim.Net <> nil then
    Net := Prim.Net.name;

  PadType := 'smd';

  PnPout.Add('"layers":' + PadLayer + ',');
  if PadPin1 then
    PnPout.Add('"pin1":1,');
  PnPout.Add('"pos":' + '[' + PadX + ', ' + PadY + ']' + ',');
  PnPout.Add('"size":' + '[' + PadWidth + ', ' + PadHeight + ']' + ',');
  PnPout.Add('"angle":' + PadAngle + ',');
  PnPout.Add('"shape":' + JSONStrToStr(PadShape) + ',');
  PnPout.Add('"type":' + JSONStrToStr(PadType));

  (*
    PnPout.Add('"Layer":' + JSONStrToStr(PadLayer) + ',');
    PnPout.Add('"Type":' + JSONStrToStr(PadType) + ',');
    PnPout.Add('"Shape":' + JSONStrToStr(PadShape) + ',');
    if PadType = 'th' then
    begin
    PnPout.Add('"DrillShape":' + JSONStrToStr(PadDrillShape) + ',');
    PnPout.Add('"DrillWidth":' + (PadDrillWidth) + ',');
    PnPout.Add('"DrillHeight":' + (PadDrillHeight) + ',');
    end;
    // PnPout.Add('"Debug":'+'"'+Preprocess(Prim.Layer)+'"'+',');
    PnPout.Add('"X":' + (PadX) + ',');
    PnPout.Add('"Y":' + (PadY) + ',');
    PnPout.Add('"Width":' + (PadWidth) + ',');
    PnPout.Add('"Height":' + (PadHeight) + ',');
    PnPout.Add('"Angle":' + (PadAngle) + ',');
    PnPout.Add('"Net":' + JSONStrToStr(Net));
  *)

  PnPout.Add('}');
  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseFootprintGeneric(Board: IPCB_Board; Component: TObject;
  SelectedFields, SelectedGroupParameters: TStringList; NoBOM: Boolean): String;
var
  PnPout: TStringList;
  Iterator: IPCB_BoardIterator;
  ComponentIterator: IPCB_GroupIterator;

  Pad: IPCB_Pad;

  x, Y, Rotation, Layer, Net: TString;

  Iter, Prim: TObject;
  PadsCount: Integer;
  X1, Y1, X2, Y2, _W, _H: Single;
  Width, Height: String;

  sl: TStringList;
  hhhhi: Integer;
  hhhh: String;
begin
  PnPout := TStringList.Create;

  If (Component.Layer = eTopLayer) Then
    Layer := 'F'
  Else
    Layer := 'B';
  x := JSONFloatToStr(CoordToMMs(Component.x - Board.XOrigin));
  Y := JSONFloatToStr(-CoordToMMs(Component.Y - Board.YOrigin));
  Rotation := IntToStr(Component.Rotation);

  // TODO: Is it correct? X1,Y1 vs X,Y
  X1 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Left -
    Board.XOrigin);
  Y1 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Bottom -
    Board.YOrigin);
  X2 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Right -
    Board.XOrigin);
  Y2 := CoordToMMs(Component.BoundingRectangleNoNameCommentForSignals.Top -
    Board.YOrigin);

  Width := JSONFloatToStr(X2 - X1);
  Height := JSONFloatToStr(Y2 - Y1);

  x := StringReplace(FloatToStr(X1), ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));
  Y := StringReplace(FloatToStr(-Y2), ',', '.',
    MkSet(rfReplaceAll, rfIgnoreCase));

  {
    bbox['pos'] :=[x0.round(), -y1.round()]; //
    bbox['relpos'] :=[0, 0];
    bbox['angle'] :=0;
    bbox['size'] :=[(x1 - x0).round(), (y1 - y0).round()];
    bbox['center'] :=[(x0 + bbox.size[0] / 2).round(), -(y0 + bbox.size[1] / 2).round()];
  }

  PnPout.Add('{');

  PnPout.Add('"ref":' + JSONStrToStr(Component.SourceDesignator) + ',');
  PnPout.Add('"center":' + '[0, 0]' + ',');
  PnPout.Add('"bbox": {');
  PnPout.Add('"pos":' + '[' + x + ', ' + Y + ']' + ',');
  PnPout.Add('"relpos":' + '[0, 0]' + ',');
  PnPout.Add('"size":' + '[' + Width + ', ' + Height + ']' + ',');
  PnPout.Add('"angle":' + '0');
  PnPout.Add('},');
  PnPout.Add('"pads": [');

  (*
    PnPout.Add('"Designator":' + JSONStrToStr(Component.SourceDesignator) + ',');
    PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');

    sl := GetComponentParameters(Component);

    PnPout.Add('"PartNumber":' + JSONStrToStr
    (sl.Values[ValueParameterName]) + ',');
    PnPout.Add('"Value":' + JSONStrToStr(sl.Values[ValueParameterName]) + ',');

    PnPout.Add('"X":' + (x) + ',');
    PnPout.Add('"Y":' + (Y) + ',');
    PnPout.Add('"Width":' + (Width) + ',');
    PnPout.Add('"Height":' + (Height) + ',');
    PnPout.Add('"NoBOM":' + JSONBoolToStr(NoBOM) + ',');

    PnPout.Add('"Pads":' + '['); *)

  PadsCount := 0;
  Iter := Component.GroupIterator_Create;
  Iter.AddFilter_ObjectSet(MkSet(ePadObject));
  Iter.AddFilter_LayerSet(AllLayers);
  Prim := Iter.FirstPCBObject;
  while (Prim <> nil) do
  begin
    Pad := Prim;
    Inc(PadsCount);
    If (PadsCount > 1) Then
      PnPout.Add(',');

    PnPout.Add(ParsePadGeneric(Board, Prim, Pad));

    // pads :=pads.concat(parsePad(Prim));
    // if (isSMD and (Prim.Layer = eMultiLayer)) then begin
    // isSMD :=false;
    // end;
    Prim := Iter.NextPCBObject;
  end;
  Component.GroupIterator_Destroy(Iter);

  PnPout.Add('],');

  PnPout.Add('"drawings": [],');
  PnPout.Add('"layer":' + JSONStrToStr(Layer));
  PnPout.Add('}');
  Result := PnPout.Text;
  PnPout.Free;
end;

function ParseRegionGeneric(Board: IPCB_Board; Prim: TObject;
  NoType: Boolean): String;
var
  PnPout: TStringList;
  Layer, Net: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  // EdgeWidth, EdgeX1, EdgeY1, PadDrillWidth: String;
  EdgeX1, EdgeY1: String;
  EdgeType: String;
  k: Integer;
  CI1, CO1: TObject;
  contour: IPCB_Contour;
  kk: Integer;
begin
  PnPout := TStringList.Create;

  If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
  Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
  Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer';
  Net := 'No Net';
  if Prim.Net <> nil then
    Net := Prim.Net.name;

  EdgeType := 'polygon';
  PnPout.Add('{');
  if not NoType then
  begin
    PnPout.Add('"type":' + JSONStrToStr(EdgeType) + ',');
    PnPout.Add('"pos":' + '[0,0]' + ',');
    PnPout.Add('"angle":' + '0' + ',');
  end;
  // PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  // PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  // PnPout.Add('"Net":' + JSONStrToStr(Net) + ',');
  PnPout.Add('"polygons": [');
  // PnPout.Add('}');

  k := 0;

  // CI1 := Prim.GroupIterator_Create;
  // CI1.AddFilter_ObjectSet(MkSet(ePadObject));
  // CO1 := CI1.FirstPCBObject;
  // While (CO1 <> Nil) Do
  CO1 := Prim;

  // if (CO1<>nil) then
  begin
    if CO1.ObjectId = eRegionObject then
    begin
      Inc(k);
      If (k > 1) Then
        PnPout.Add(',');
      PnPout.Add('[');
      contour := CO1.GetMainContour();
      for kk := 0 to contour.Count do
      begin
        If (kk > 0) Then
          PnPout.Add(',');
        EdgeX1 := JSONFloatToStr
          (CoordToMMs(contour.GetState_PointX(kk mod contour.Count) -
          Board.XOrigin));
        EdgeY1 := JSONFloatToStr
          (-CoordToMMs(contour.GetState_PointY(kk mod contour.Count) -
          Board.YOrigin));
        PnPout.Add('[');
        PnPout.Add(EdgeX1 + ',');
        PnPout.Add(EdgeY1);
        PnPout.Add(']');
      end;
      PnPout.Add(']');
    end;
    // CO1 := CI1.NextPCBObject;
  end;
  // Prim.GroupIterator_Destroy(CI1);

  PnPout.Add(']');

  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function ParsePolyGeneric(Board: IPCB_Board; Prim: TObject;
  NoType: Boolean): String;
var
  PnPout: TStringList;
  Layer, Net: String;
  // X1, Y1, X2, Y2, _W, _H: Single;
  // EdgeWidth, EdgeX1, EdgeY1, PadDrillWidth: String;
  EdgeX1, EdgeY1: String;
  EdgeType: String;
  k: Integer;
  CI1, CO1: TObject;
  contour: IPCB_Contour;
  kk: Integer;
begin
  PnPout := TStringList.Create;

  If (Prim.Layer = eTopOverlay) Then
    Layer := 'TopOverlay'
  Else If (Prim.Layer = eBottomOverlay) Then
    Layer := 'BottomOverlay'
  Else If (Prim.Layer = eTopLayer) Then
    Layer := 'TopLayer'
  Else If (Prim.Layer = eBottomLayer) Then
    Layer := 'BottomLayer';
  Net := 'No Net';
  if Prim.Net <> nil then
    Net := Prim.Net.name;

  EdgeType := 'polygon';
  PnPout.Add('{');
  if not NoType then
  begin
    PnPout.Add('"type":' + JSONStrToStr(EdgeType) + ',');
    PnPout.Add('"pos":' + '[0,0]' + ',');
    PnPout.Add('"angle":' + '0' + ',');
  end;
  // PnPout.Add('"Layer":' + JSONStrToStr(Layer) + ',');
  // PnPout.Add('"Type":' + JSONStrToStr(EdgeType) + ',');
  // PnPout.Add('"Net":' + JSONStrToStr(Net) + ',');
  PnPout.Add('"polygons": [');

  k := 0;

  CI1 := Prim.GroupIterator_Create;
  // CI1.AddFilter_ObjectSet(MkSet(ePadObject));
  CO1 := CI1.FirstPCBObject;
  While (CO1 <> Nil) Do
  begin
    if CO1.ObjectId = eRegionObject then
    begin
      Inc(k);
      If (k > 1) Then
        PnPout.Add(',');
      PnPout.Add('[');
      contour := CO1.GetMainContour();
      for kk := 0 to contour.Count do
      begin
        If (kk > 0) Then
          PnPout.Add(',');
        EdgeX1 := JSONFloatToStr
          (CoordToMMs(contour.GetState_PointX(kk mod contour.Count) -
          Board.XOrigin));
        EdgeY1 := JSONFloatToStr
          (-CoordToMMs(contour.GetState_PointY(kk mod contour.Count) -
          Board.YOrigin));
        PnPout.Add('[');
        PnPout.Add(EdgeX1 + ',');
        PnPout.Add(EdgeY1);
        PnPout.Add(']');
      end;
      PnPout.Add(']');
    end;
    CO1 := CI1.NextPCBObject;
  end;
  Prim.GroupIterator_Destroy(CI1);

  PnPout.Add(']');

  PnPout.Add('}');

  Result := PnPout.Text;
  PnPout.Free;
end;

function PickAndPlaceOutputGeneric: String;
var
  Board: IPCB_Board; // document board object
  Component: IPCB_Component; // component object
  Iterator: IPCB_BoardIterator;
  SMDcomponent: Boolean;
  BoardUnits: String;
  // Current unit string mm/mils
  PnPout: TStringList;
  Count: Integer;
  FileName: TString;
  Document: IServerDocument;
  pcbDocPath: TString;
  flagRequirePcbDocFile: Boolean;
  Separator: TString;
  Iter, Prim: TObject;
  PadsCount: Integer;
  X1, Y1, X2, Y2, _W, _H: Single;
  Width, Height: String;
  CurrParm: IParameter;
  NoBOM: Boolean;
  ccc: IComponent;
  Edges: String;

  EdgeWidth, EdgeX1, EdgeY1, EdgeX2, EdgeY2, EdgeRadius: String;

  _Document: IServerDocument;
  // sl: TStringList;
  tmpx, tmpy: String;
  SelectedFields: TStringList;
  SelectedGroupParameters: TStringList;
  hhhhi: Integer;
  hhhh: String;
  StartTime, StopTime: TDateTime;
  Elapsed: Integer;
  Metadata: String;
  EdgesBBox: String;
  Components: String;
  TracksF: String;
  TracksB: String;
  ZonesF: String;
  ZonesB: String;
  Footprints: String;
  SilkscreenF: String;
  SilkscreenB: String;
Begin
  // Make sure the current Workspace opens or else quit this script
  CurrWorkSpace := GetWorkSpace;
  If (CurrWorkSpace = Nil) Then
    Exit;

  // Make sure the currently focussed Project in this Workspace opens or else
  // quit this script
  CurrProject := CurrWorkSpace.DM_FocusedProject;
  If CurrProject = Nil Then
    Exit;

  flagRequirePcbDocFile := True;

  FindProjectPcbDocFile(CurrProject, flagRequirePcbDocFile,
    { var } pcbDocPath);

  // TODO: Close
  _Document := Client.OpenDocument('pcb', pcbDocPath);
  Board := PCBServer.GetPCBBoardByPath(pcbDocPath);

  If Not Assigned(Board) Then // check of active document
  Begin
    ShowMessage('The Current Document is not a PCB Document.');
    Exit;
  End;

  StartTime := Now();

  SelectedFields := GetSelectedFields();
  SelectedGroupParameters := GetSelectedGroupParameters();

  Iterator := Board.BoardIterator_Create;
  Iterator.AddFilter_ObjectSet(MkSet(eComponentObject));
  Iterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
  Iterator.AddFilter_Method(eProcessAll);

  Separator := GetSeparator();
  Count := 0;
  PnPout := TStringList.Create;
  Component := Iterator.FirstPCBObject;

  While (Component <> Nil) Do
  Begin
    NoBOM := False;
    ccc := GetCompFromCompEx(Component);
    CurrParm := ccc.DM_GetParameterByName('Component Kind');
    // (CurrParm <> nil) and
    if (CurrParm.DM_Value = 'Standard (No BOM)') then
    begin
      NoBOM := True;
    end;

    // Print Pick&Place data of SMD components to file
    if ComponentIsFittedInCurrentVariant(Component.SourceUniqueId, Component.SourceDesignator,
      ProjectVariant) then
      if (LayerFilterIndex = 0) or
        ((LayerFilterCb = 1) and (Component.Layer = eTopLayer)) or
        ((LayerFilterCb = 2) and (Component.Layer = eBottomLayer)) then
      Begin
        Inc(Count);
        If (Count > 1) Then
        begin
          Components := Components + ','; // PnPout.Add(',');
          Footprints := Footprints + ','; // PnPout.Add(',');
        end;

        Components := Components + ParseComponentGeneric(Board, Component,
          SelectedFields, SelectedGroupParameters, NoBOM);
        Footprints := Footprints + ParseFootprintGeneric(Board, Component,
          SelectedFields, SelectedGroupParameters, NoBOM);

        // PnPout.Add(ParseComponent(Board, Component, SelectedFields, SelectedGroupParameters, NoBOM));
      End;
    Component := Iterator.NextPCBObject;
  End;
  Board.BoardIterator_Destroy(Iterator);

  // PnPout.Add('],');
  // PnPout.Add('"Board":[');

  Edges := '';

  Count := 0;

  Iter := Board.BoardIterator_Create;
  Iter.AddFilter_LayerSet(MkSet(eKeepOutLayer));
  Iter.AddFilter_ObjectSet(MkSet(eArcObject, eTrackObject));
  Iter.AddFilter_Method(eProcessAll);
  Prim := Iter.FirstPCBObject;
  while (Prim <> nil) do
  begin
    Inc(Count);
    If (Count > 1) Then
      Edges := Edges + ', ';

    case (Prim.ObjectId) of
      eArcObject:
        begin
          Edges := Edges + ParseArcGeneric(Board, Prim);
        end;
      eTrackObject:
        begin
          Edges := Edges + ParseTrackGeneric(Board, Prim, False);
        end;
    end;

    Prim := Iter.NextPCBObject;
  end;
  Board.BoardIterator_Destroy(Iter);
  (*
    PnPout.Add('],');
    PnPout.Add('"BB":{');

    // var bbox = {};
    // bbox["minx"] = CoordToMMs(Board.BoardOutline.BoundingRectangle.Left).round();
    // bbox["miny"] = -CoordToMMs(pcb.board.BoardOutline.BoundingRectangle.Top).round();
    // bbox["maxx"] = CoordToMMs(pcb.board.BoardOutline.BoundingRectangle.Right).round();
    // bbox["maxy"] = -CoordToMMs(pcb.board.BoardOutline.BoundingRectangle.Bottom).round();
  *)

  EdgeX1 := JSONFloatToStr(CoordToMMs(Board.BoardOutline.BoundingRectangle.Left
    - Board.XOrigin));
  EdgeY1 := JSONFloatToStr(-CoordToMMs(Board.BoardOutline.BoundingRectangle.Top
    - Board.YOrigin));
  EdgeX2 := JSONFloatToStr(CoordToMMs(Board.BoardOutline.BoundingRectangle.Right
    - Board.XOrigin));
  EdgeY2 := JSONFloatToStr
    (-CoordToMMs(Board.BoardOutline.BoundingRectangle.Bottom - Board.YOrigin));
  {
    EdgeX1 := JSONFloatToStr(CoordToMMs(Board.BoardOutline.BoundingRectangle.Left
    - 0));
    EdgeY1 := JSONFloatToStr(-CoordToMMs(Board.BoardOutline.BoundingRectangle.Top
    - 0));
    EdgeX2 := JSONFloatToStr(CoordToMMs(Board.BoardOutline.BoundingRectangle.Right
    - 0));
    EdgeY2 := JSONFloatToStr
    (-CoordToMMs(Board.BoardOutline.BoundingRectangle.Bottom - 0));
  }
  EdgesBBox := '';

  EdgesBBox := EdgesBBox + '"minx":' + (EdgeX1) + ',';
  EdgesBBox := EdgesBBox + '"miny":' + (EdgeY1) + ',';
  EdgesBBox := EdgesBBox + '"maxx":' + (EdgeX2) + ',';
  EdgesBBox := EdgesBBox + '"maxy":' + (EdgeY2);


  // PnPout.Add('},');
  // PnPout.Add('"Extra":[');

  Count := 0;

  TracksF := '';
  TracksB := '';
  SilkscreenF := '';
  SilkscreenB := '';
  ZonesF := '';
  ZonesB := '';

  Iter := Board.BoardIterator_Create;
  Iter.AddFilter_LayerSet(MkSet(eTopOverlay, eBottomOverlay, eTopLayer,
    eBottomLayer, eMultiLayer));
  Iter.AddFilter_ObjectSet(MkSet(eArcObject, eTrackObject, eTextObject,
    ePolyObject, eRegionObject, eViaObject));
  Iter.AddFilter_Method(eProcessAll);
  Prim := Iter.FirstPCBObject;
  while (Prim <> nil) do
  begin
    // TODO: UGLY
    if (Prim.ObjectId = eTextObject) and (Prim.IsHidden) then
    begin
      Prim := Iter.NextPCBObject;
      continue;
    end;
    if ((Prim.Layer = eTopLayer) or (Prim.Layer = eBottomLayer)) and
      (Prim.ObjectId <> eTrackObject) and (Prim.ObjectId <> eViaObject) and
      (Prim.ObjectId <> ePolyObject) and (Prim.ObjectId <> eFillObject) and
      (Prim.ObjectId <> eRegionObject) then
    begin
      Prim := Iter.NextPCBObject;
      continue;
    end;
    if (Prim.ObjectId = eRegionObject) and
      ((Prim.Kind() <> eRegionKind_Copper) or Prim.InPolygon()) then
    begin
      Prim := Iter.NextPCBObject;
      continue;
    end;
    if ((Prim.ObjectId = ePolyObject)) and
      ((Prim.Layer <> eTopLayer) and (Prim.Layer <> eBottomLayer)) then
    begin
      Prim := Iter.NextPCBObject;
      continue;
    end;

    // Inc(Count);
    // If (Count > 1) Then
    // PnPout.Add(',');

    case (Prim.ObjectId) of
      eTextObject:
        begin
          // PnPout.Add(ParseText(Board, Prim));
        end;
      eArcObject:
        begin
          // PnPout.Add(ParseArc2(Board, Prim));
        end;
      eTrackObject:
        begin
          If (Prim.Layer = eTopOverlay) Then
          begin
            if Length(SilkscreenF) > 0 then
              SilkscreenF := SilkscreenF + ', ';
            SilkscreenF := SilkscreenF + ParseTrackGeneric(Board, Prim, False);
          end;
          If (Prim.Layer = eBottomOverlay) Then
          begin
            if Length(SilkscreenB) > 0 then
              SilkscreenB := SilkscreenB + ', ';
            SilkscreenB := SilkscreenB + ParseTrackGeneric(Board, Prim, False);
          end;
          If (Prim.Layer = eTopLayer) Then
          begin
            if Length(TracksF) > 0 then
              TracksF := TracksF + ', ';
            TracksF := TracksF + ParseTrackGeneric(Board, Prim, True);
          end;
          If (Prim.Layer = eBottomLayer) Then
          begin
            if Length(TracksB) > 0 then
              TracksB := TracksB + ', ';
            TracksB := TracksB + ParseTrackGeneric(Board, Prim, True);
          end;
        end;
      eViaObject:
        begin

          // If (Prim.Layer = eTopLayer) Then
          begin
            if Length(TracksF) > 0 then
              TracksF := TracksF + ', ';
            TracksF := TracksF + ParseVIAGeneric(Board, Prim, True);
          end;

          // If (Prim.Layer = eBottomLayer) Then
          begin
            if Length(TracksB) > 0 then
              TracksB := TracksB + ', ';
            TracksB := TracksB + ParseVIAGeneric(Board, Prim, True);
          end;
        end;
      eRegionObject:
        begin
          If (Prim.Layer = eTopOverlay) Then
          begin
            if Length(SilkscreenF) > 0 then
              SilkscreenF := SilkscreenF + ', ';
            SilkscreenF := SilkscreenF + ParseRegionGeneric(Board, Prim, False);
          end;
          If (Prim.Layer = eBottomOverlay) Then
          begin
            if Length(SilkscreenB) > 0 then
              SilkscreenB := SilkscreenB + ', ';
            SilkscreenB := SilkscreenB + ParseRegionGeneric(Board, Prim, False);
          end;
          If (Prim.Layer = eTopLayer) Then
          begin
            if Length(ZonesF) > 0 then
              ZonesF := ZonesF + ', ';
            ZonesF := ZonesF + ParseRegionGeneric(Board, Prim, True);
          end;
          If (Prim.Layer = eBottomLayer) Then
          begin
            if Length(ZonesB) > 0 then
              ZonesB := ZonesB + ', ';
            ZonesB := ZonesB + ParseRegionGeneric(Board, Prim, True);
          end;
        end;
      ePolyObject:
        begin
          If (Prim.Layer = eTopOverlay) Then
          begin
            if Length(SilkscreenF) > 0 then
              SilkscreenF := SilkscreenF + ', ';
            SilkscreenF := SilkscreenF + ParsePolyGeneric(Board, Prim, False);
          end;
          If (Prim.Layer = eBottomOverlay) Then
          begin
            if Length(SilkscreenB) > 0 then
              SilkscreenB := SilkscreenB + ', ';
            SilkscreenB := SilkscreenB + ParsePolyGeneric(Board, Prim, False);
          end;
          If (Prim.Layer = eTopLayer) Then
          begin
            if Length(ZonesF) > 0 then
              ZonesF := ZonesF + ', ';
            ZonesF := ZonesF + ParsePolyGeneric(Board, Prim, True);
          end;
          If (Prim.Layer = eBottomLayer) Then
          begin
            if Length(ZonesB) > 0 then
              ZonesB := ZonesB + ', ';
            ZonesB := ZonesB + ParsePolyGeneric(Board, Prim, True);
          end;
        end;
    end;

    Prim := Iter.NextPCBObject;
  end;
  Board.BoardIterator_Destroy(Iter);

  // PnPout.Add('],');
  // PnPout.Add('"Metadata":{');*)

  Metadata := '';

  Metadata := Metadata + '"title":' + JSONStrToStr(Title) + ',';
  Metadata := Metadata + '"revision":' + JSONStrToStr(Revision) + ',';
  Metadata := Metadata + '"company":' + JSONStrToStr(Company) + ',';
  Metadata := Metadata + '"date":' + JSONStrToStr('todo');

  (* PnPout.Add('},');
    PnPout.Add('"Settings":{');
    PnPout.Add('"AddNets":' + JSONBoolToStr(AddNets) + ',');
    PnPout.Add('"AddTracks":' + JSONBoolToStr(AddTracks) + ',');

    PnPout.Add('"Fields":' + '[');

    for hhhhi := 0 to SelectedFields.Count - 1 do
    begin
    if (hhhhi > 0) then
    PnPout.Add(',');
    hhhh := SelectedFields[hhhhi];
    PnPout.Add(JSONStrToStr(hhhh));
    end;
    PnPout.Add(']');

    PnPout.Add('}');
    PnPout.Add('}');

  *)

  PnPout := TStringList.Create;
  PnPout.Add('{');
  PnPout.Add('"spec_version": 1,');
  PnPout.Add('"pcbdata": {');
  PnPout.Add('"edges_bbox": {' + EdgesBBox + '},');
  PnPout.Add('"edges": [' + Edges + '],');

  PnPout.Add('"drawings": {');

  PnPout.Add('"silkscreen": {');
  PnPout.Add('"F": [' + SilkscreenF + '],');
  PnPout.Add('"B": [' + SilkscreenB + ']');
  PnPout.Add('},');

  PnPout.Add('"fabrication": {');
  PnPout.Add('"F": [' + '' + '],');
  PnPout.Add('"B": [' + '' + ']');
  PnPout.Add('}');

  PnPout.Add('},');

  PnPout.Add('"footprints": [' + Footprints + '],');
  PnPout.Add('"metadata": {' + Metadata + '},');

  PnPout.Add('"tracks": {');
  PnPout.Add('"F": [' + TracksF + '],');
  PnPout.Add('"B": [' + TracksB + ']');
  PnPout.Add('},');

  PnPout.Add('"zones": {');
  PnPout.Add('"F": [' + ZonesF + '],');
  PnPout.Add('"B": [' + ZonesB + ']');
  PnPout.Add('},');

  PnPout.Add('"nets": [],');
  PnPout.Add('"font_data": {}');
  PnPout.Add('},');
  PnPout.Add('"components": [' + Components + ']');
  PnPout.Add('}');

  Result := PnPout.Text;

  PnPout.Free;

  {
    Document  := Client.OpenDocument('Text', FileName);
    If Document <> Nil Then
    Client.ShowDocument(Document);

    ShowMessage(IntToStr(Count) + ' were exported to Pick and Place file:' + #13 + Board.FileName + '.pic');
  }
  StopTime := Now();
  Elapsed := Trunc((StopTime - StartTime) * 86400 * 1000);
  // ShowMessage('Script execution complete in ' + IntToStr(Elapsed) + 'ms');
End;

{.....................................................................................................................}
{.                                              Generic JSON Generation                                              .}
{.....................................................................................................................}

function StringLoadFromFile(FileName: String): String;
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.LoadFromFile(FileName);
  Result := s.Text;
  s.Free;
end;

function ReplaceEx(a, c, FileName: String): String;
var
  s: TStringList;
  e: String;
  i: Integer;
begin
  s := TStringList.Create;
  s.LoadFromFile(FileName);
  e := s.Text;
  s.Free;

  Result := StringReplace(a, '///' + c + '///', e,
    MkSet(rfReplaceAll, rfIgnoreCase));
  // Result := StringReplace(a, '///'+'SPLITJS', e, MkSet(rfReplaceAll,rfIgnoreCase));
end;

function ReplaceEx2(a, c, e: String): String;
begin

  Result := StringReplace(a, '///' + c + '///', e,
    MkSet(rfReplaceAll, rfIgnoreCase));
  // Result := StringReplace(a, '///'+'SPLITJS', e, MkSet(rfReplaceAll,rfIgnoreCase));
end;


{.....................................................................................................................}
{.                                               Output File Generation                                              .}
{.....................................................................................................................}


procedure GenerateHTML(pcbdata);
var
  s: TStringList;
  Data: String;
begin
  // Load template into Data variable
  s := TStringList.Create;
  s.LoadFromFile(GetWDFileName('web\ibom.html'));
  Data := s.Text;
  s.Free;

  // Replace template sections with contents of files and/or content from the PCB board
  Data := ReplaceEx(Data, 'CSS', GetWDFileName('web\ibom.css'));
  Data := ReplaceEx(Data, 'USERCSS',
    GetWDFileName('web\user-file-examples\user.css'));
  Data := ReplaceEx(Data, 'SPLITJS', GetWDFileName('web\split.js'));
  Data := ReplaceEx(Data, 'LZ-STRING', GetWDFileName('web\lz-string.js'));
  Data := ReplaceEx(Data, 'POINTER_EVENTS_POLYFILL',
    GetWDFileName('web\pep.js'));
  Data := ReplaceEx2(Data, 'CONFIG', GenerateNativeConfig());
  Data := ReplaceEx2(Data, 'PCBDATA', pcbdata + #13#10 +
    StringLoadFromFile(GetWDFileName('altium-pcbdata.js')));
  Data := ReplaceEx(Data, 'UTILJS', GetWDFileName('web\util.js'));
  Data := ReplaceEx(Data, 'RENDERJS', GetWDFileName('web\render.js'));
  Data := ReplaceEx(Data, 'TABLEUTILJS', GetWDFileName('web\table-util.js'));
  Data := ReplaceEx(Data, 'IBOMJS', GetWDFileName('web\ibom.js'));
  Data := ReplaceEx2(Data, 'USERJS',
    StringLoadFromFile(GetWDFileName('web\user-file-examples\user.js')) + #13#10
    + StringLoadFromFile(GetWDFileName('altium-user.js')));
  Data := ReplaceEx(Data, 'USERHEADER',
    GetWDFileName('web\user-file-examples\userheader.html'));
  Data := ReplaceEx(Data, 'USERFOOTER',
    GetWDFileName('web\user-file-examples\userfooter.html'));

  // Save the manipulated HTML to the output file
  s := TStringList.Create;
  s.Text := Data;
  s.SaveToFile(GetOutputFileNameWithExtension('.html'));
  s.Free;
end;


function GenerateNativeConfig: String;
var
  PnPout: TStringList;
  s: TStringList;
  i: Integer;
Begin
  PnPout := TStringList.Create;

  PnPout.Add('var config = {');
  PnPout.Add('"show_fabrication":' + JSONBoolToStr(FabLayer) + ',');
  PnPout.Add('"redraw_on_drag":' + JSONBoolToStr(True) + ',');
  PnPout.Add('"highlight_pin1":' + JSONBoolToStr(Highlighting1Pin) + ',');
  PnPout.Add('"offset_back_rotation":' + JSONBoolToStr(False) + ',');
  PnPout.Add('"kicad_text_formatting":' + JSONBoolToStr(True) + ',');
  PnPout.Add('"dark_mode":' + JSONBoolToStr(DarkMode) + ',');
  PnPout.Add('"bom_view":' + JSONStrToStr('left-right') + ',');
  PnPout.Add('"board_rotation":' + JSONStrToStr('0.0') + ',');
  PnPout.Add('"checkboxes":' + JSONStrToStr('Sourced,Placed') + ',');
  PnPout.Add('"show_silkscreen":' + JSONBoolToStr(True) + ',');
  PnPout.Add('"fields":' + '[');

  s := GetSelectedFields();
  for i := 0 to s.Count - 1 do
  begin
    if i > 0 then
      PnPout.Add(',');

    PnPout.Add(JSONStrToStr(s[i]));
  end;

  PnPout.Add(']' + ',');
  PnPout.Add('"show_pads":' + JSONBoolToStr(True) + ',');
  PnPout.Add('"layer_view":' + JSONStrToStr('FB') + ',');
  PnPout.Add('};');

  Result := PnPout.Text;

  PnPout.Free;
end;


procedure DumpAsJS(pcbdata);
var
  s: TStringList;
  Data: String;
begin
  s := TStringList.Create;
  s.Text := pcbdata;
  s.SaveToFile(GetOutputFileNameWithExtension('.js'));
  s.Free;
end;

procedure DumpAsJSON(pcbdata);
var
  s: TStringList;
  Data: String;
begin
  s := TStringList.Create;
  s.Text := pcbdata;
  s.SaveToFile(GetOutputFileNameWithExtension('.json'));
  s.Free;
end;

{.....................................................................................................................}
{.                                                   State Handling                                                  .}
{.....................................................................................................................}

Procedure Initialize;
Begin
  // Open Workspace, Project, Get Variants, etc.
  InitializeProject();
End;

Procedure InitializeProject;
Var
  ProjVarIndex: Integer; // Index for iterating through variants
Begin
  // Make sure the current Workspace opens or else quit this script
  CurrWorkSpace := GetWorkSpace;
  If (CurrWorkSpace = Nil) Then
    Exit;

  // Make sure the currently focussed Project in this Workspace opens or else
  // quit this script
  CurrProject := CurrWorkSpace.DM_FocusedProject;
  If CurrProject = Nil Then
    Exit;

  // [!!!]
  SetupProjectVariant();
  {
    // Determine how many Assembly Variants are defined within this focussed Project
    ProjectVariantCount := CurrProject.DM_ProjectVariantCount;

    // Process each Project Assembly Variant sequentially
    For ProjVarIndex := 0 To ProjectVariantCount - 1 Do
    Begin
    // Fetch the currently indexed project Assembly Variant
    ProjectVariant := CurrProject.DM_ProjectVariants[ ProjVarIndex ];
    VariantsComboBox.Items.Add( ProjectVariant.DM_Description );
    End;

    // Choose first variant to start with
    VariantsComboBox.ItemIndex := 0;
    ProjectVariant := CurrProject.DM_ProjectVariants[ 0 ];
  }
End;

procedure SetupProjectVariant;
Var
  ProjVarIndex: Integer; // Index for iterating through variants
  TempVariant: IProjectVariant; // A temporary Handle for a ProjectVariant
Begin
  ProjectVariant := CurrProject.DM_CurrentProjectVariant;
  {
    // Determine how many ProjectVariants are defined within this focussed Project
    ProjectVariantCount := CurrProject.DM_ProjectVariantCount;
    ProjectVariant := Nil;

    // Find the Project Variant matching the Variants Combo-Box
    For ProjVarIndex := 0 To ProjectVariantCount - 1 Do
    Begin
    // Fetch the currently indexed project Assembly Variant
    TempVariant := CurrProject.DM_ProjectVariants[ ProjVarIndex ];

    // See if the Description matches that in the Variants Combo-Box
    If (VariantName = TempVariant.DM_Description)
    Then ProjectVariant := CurrProject.DM_ProjectVariants[ ProjVarIndex ];
    End;
  }
End;

{
 Transfers the global state variables into the Form object values
}
procedure SetState_Controls;
var
  i: Integer;
  tmpn: String;
  ii: Integer;
Begin
  LayerFilterCb.ItemIndex := LayerFilterIndex;
  FormatCb.ItemIndex := FormatIndex;
  // FieldSeparatorCb.ItemIndex := FieldSeparatorIndex;
  DarkModeChk.Checked := DarkMode;
  AddNetsChk.Checked := AddNets;
  AddTracksChk.Checked := AddTracks;
  Highlighting1PinChk.Checked := Highlighting1Pin;
  FabLayerChk.Checked := FabLayer;
  TitleEdt.Text := Title;
  CompanyEdt.Text := Company;
  RevisionEdt.Text := Revision;
  ValueParameterCb.ItemIndex := ValueParameterCb.Items.IndexOf
    (ValueParameterName);
  for i := 0 to ColumnsParametersNames.Count - 1 do
  begin
    if ColumnsParametersClb.Items.IndexOf(ColumnsParametersNames[i]) <> -1 then
    begin
      ColumnsParametersClb.Checked
        [ColumnsParametersClb.Items.IndexOf(ColumnsParametersNames[i])] := True;
    end;
  end;
  for i := 0 to GroupParametersNames.Count - 1 do
  begin
    if GroupParametersClb.Items.IndexOf(GroupParametersNames[i]) <> -1 then
    begin
      GroupParametersClb.Checked
        [GroupParametersClb.Items.IndexOf(GroupParametersNames[i])] := True;
    end;
  end;
  /// //////////////////////////////////////////////////////////
  {
    ParametersComboBox          .ItemIndex := ParametersComboBox.Items.IndexOf(ParameterName);
    VariantsComboBox            .ItemIndex := VariantsComboBox  .Items.IndexOf(VariantName);
  }
End;

{
  "State" is the values in the global variables, defined at the top.
  At the very first call to Configure, `Paramters` is empty, thus the defaults
  defined here come into effect. Otherwise, we overwrite each default value
  with what we find in the Paramters list.
}
Procedure SetState_FromParameters(AParametersList: String);
Var
  s: String;
Begin
  InitializeProject();

  // Defaults definition
  {
    UseParameters        := False;
    UseVariants          := False;
    OpenOutputs          := True;
    AddToProject         := True; }
  TargetFolder := '';
  TargetFileName := '';
  TargetPrefix := '';
  LayerFilterIndex := 0;
  FormatIndex := 0;
  FieldSeparatorIndex := 0;
  PluginExecutable := 'gostbomkompas.exe';
  DarkMode := False;
  AddNets := False;
  AddTracks := False;
  Highlighting1Pin := False;
  FabLayer := False;
  Title := 'Title';
  Company := 'Company';
  Revision := 'Revision: 1';
  ValueParameterName := 'Value';
  ColumnsParametersNames := TStringList.Create;
  ColumnsParametersNames.Delimiter := ',';
  ColumnsParametersNames.StrictDelimiter := True;
  ColumnsParametersNames.Add('Value');
  ColumnsParametersNames.Add('[Footprint]');

  GroupParametersNames := TStringList.Create;
  GroupParametersNames.Delimiter := ',';
  GroupParametersNames.StrictDelimiter := True;
  GroupParametersNames.Add('Value');
  GroupParametersNames.Add('[Footprint]');

  // Overwrite defaults (if values are provided in Parameter list)
  {
    If GetState_Parameter(AParametersList, 'ParameterName'       , S) Then ParameterName        := S;
    If GetState_Parameter(AParametersList, 'VariantName'         , S) Then VariantName          := S;
  }
  If GetState_Parameter(AParametersList, 'TargetFileName', s) Then
    TargetFileName := s + '.PrjPcb';
  If GetState_Parameter(AParametersList, 'TargetFolder', s) Then
    TargetFolder := s;
  If GetState_Parameter(AParametersList, 'TargetPrefix', s) Then
    TargetPrefix := s;
  If GetState_Parameter(AParametersList, 'LayerFilterIndex', s) Then
    LayerFilterIndex := StrToInt(s);
  If GetState_Parameter(AParametersList, 'FormatIndex', s) Then
    FormatIndex := StrToInt(s);
  If GetState_Parameter(AParametersList, 'FieldSeparatorIndex', s) Then
    FieldSeparatorIndex := StrToInt(s);
  If GetState_Parameter(AParametersList, 'PluginExecutable', s) Then
    PluginExecutable := s;
  If GetState_Parameter(AParametersList, 'DarkMode', s) Then
    DarkMode := StringsEqual(s, 'True');
  If GetState_Parameter(AParametersList, 'AddNets', s) Then
    AddNets := StringsEqual(s, 'True');
  If GetState_Parameter(AParametersList, 'AddTracks', s) Then
    AddTracks := StringsEqual(s, 'True');
  If GetState_Parameter(AParametersList, 'Highlighting1Pin', s) Then
    Highlighting1Pin := StringsEqual(s, 'True');
  If GetState_Parameter(AParametersList, 'FabLayer', s) Then
    FabLayer := StringsEqual(s, 'True');
  If GetState_Parameter(AParametersList, 'Title', s) Then
    Title := s;
  If GetState_Parameter(AParametersList, 'Company', s) Then
    Company := s;
  If GetState_Parameter(AParametersList, 'Revision', s) Then
    Revision := s;
  If GetState_Parameter(AParametersList, 'ValueParameterName', s) Then
    ValueParameterName := s;
  If GetState_Parameter(AParametersList, 'ColumnsParametersNames', s) Then
    ColumnsParametersNames.DelimitedText := s;
  If GetState_Parameter(AParametersList, 'GroupParametersNames', s) Then
    GroupParametersNames.DelimitedText := s;

  // Transfer global variable state into Form objects
  SetState_Controls();
End;

{
 Transfers the Form object values into the global state variables
}
procedure GetState_Controls;
var
  i: Integer;
Begin
  {
    ParameterName         := ParametersComboBox.Items[ ParametersComboBox.ItemIndex ];
    VariantName           := VariantsComboBox  .Items[VariantsComboBox.ItemIndex];
  }
  LayerFilterIndex := LayerFilterCb.ItemIndex;
  FormatIndex := FormatCb.ItemIndex;
  // FieldSeparatorIndex := FieldSeparatorCb.ItemIndex;
  DarkMode := DarkModeChk.Checked;
  AddNets := AddNetsChk.Checked;
  AddTracks := AddTracksChk.Checked;
  Highlighting1Pin := Highlighting1PinChk.Checked;
  FabLayer := FabLayerChk.Checked;
  Title := TitleEdt.Text;
  Company := CompanyEdt.Text;
  Revision := RevisionEdt.Text;
  ValueParameterName := ValueParameterCb.Items[ValueParameterCb.ItemIndex];
  ColumnsParametersNames.Clear;
  for i := 0 to ColumnsParametersClb.Count - 1 do
  begin
    if ColumnsParametersClb.Checked[i] then
      ColumnsParametersNames.Add(ColumnsParametersClb.Items[i]);
  end;
  GroupParametersNames.Clear;
  for i := 0 to GroupParametersClb.Count - 1 do
  begin
    if GroupParametersClb.Checked[i] then
      GroupParametersNames.Add(GroupParametersClb.Items[i]);
  end;
  /// ////////////////////////////////////////////
End;

Function GetState_FromParameters: String;
Begin
  GetState_Controls();

  Result := '';
  { Result := Result +       'ParameterName='        + ParameterName;
    Result := Result + '|' + 'VariantName='          + VariantName;
  }
  Result := Result + '|' + 'LayerFilterIndex=' + IntToStr(LayerFilterIndex);
  Result := Result + '|' + 'FormatIndex=' + IntToStr(FormatIndex);
  Result := Result + '|' + 'FieldSeparatorIndex=' +
    IntToStr(FieldSeparatorIndex);
  Result := Result + '|' + 'PluginExecutable=' + PluginExecutable;
  Result := Result + '|' + 'DarkMode=' + BoolToStr(DarkMode, True);
  Result := Result + '|' + 'AddNets=' + BoolToStr(AddNets, True);
  Result := Result + '|' + 'AddTracks=' + BoolToStr(AddTracks, True);
  Result := Result + '|' + 'Highlighting1Pin=' +
    BoolToStr(Highlighting1Pin, True);
  Result := Result + '|' + 'FabLayer=' + BoolToStr(FabLayer, True);
  Result := Result + '|' + 'Title=' + Title;
  Result := Result + '|' + 'Company=' + Company;
  Result := Result + '|' + 'Revision=' + Revision;
  Result := Result + '|' + 'ValueParameterName=' + ValueParameterName;
  Result := Result + '|' + 'ColumnsParametersNames=' +
    ColumnsParametersNames.DelimitedText;
  Result := Result + '|' + 'GroupParametersNames=' +
    GroupParametersNames.DelimitedText;
End;

procedure CreateFile(F: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.SaveToFile(F);
  s.Free;
end;


{.....................................................................................................................}
{.                                                OutputJob Interface                                                .}
{.....................................................................................................................}


{
  PredictOutputFileNames should return the full path names of all files that
  will be generated by the Generate procedure, without actually generating
  them. The file names should be returned via the Result string, separated by
  '|' characters.
}
Function PredictOutputFileNames(Parameters: String): String;
Var
  OutputFileNames: TStringList;
Begin
  // Populate the global state variables from the Parameters string
  SetState_FromParameters(Parameters);

  OutputFileNames := TStringList.Create;
  OutputFileNames.Delimiter := '|';
  OutputFileNames.StrictDelimiter := True;

  If FormatIndex = 0 Then
  Begin
    OutputFileNames.Add(GetOutputFileNameWithExtension('.html'));
  End Else If FormatIndex = 1 Then Begin
    OutputFileNames.Add(GetOutputFileNameWithExtension('.js'));
  End Else If FormatIndex = 2 Then Begin
    OutputFileNames.Add(GetOutputFileNameWithExtension('.json'));
  End Else If FormatIndex = 3 Then Begin
    OutputFileNames.Add(GetOutputFileNameWithExtension('.json'));
  End Else Begin
    OutputFileNames.Add(GetOutputFileNameWithExtension('.unknown_format_choice'));
  End;

  Result := OutputFileNames.DelimitedText;
  OutputFileNames.Free;
End;

{
  Configure is the entry point for the right-click Configure command in an
  OutJob document. It shows the form with the supplied settings (encoded as a
  parameter string), and, if the user clicks OK, it returns the new settings.
  These new settings will be saved by OutJob, and applied in subsequent
  invocations of the Generate procedure.
}
Function Configure(Parameters: String): String;
Begin
  Result := '';
  Initialize;
  SetState_FromParameters(Parameters);
  RunAsOutputJob := True;
  If MainFrm.ShowModal = mrOK Then
  Begin
    Result := GetState_FromParameters();
    Close;
  End;
End;

{
  Generate is the entry point when running a Script Output from an OutJob document.
  It generates an output file(s) without showing the form. The settings to use are
  supplied from OutJob as a parameter string (whose format we can define).
}
Procedure Generate(Parameters: String);
Var
  config, jsString, jsonString: String;
  generateJS: Boolean;
Begin
  SetState_FromParameters(Parameters);

  generateJS := (FormatIndex < 2);

  If FormatIndex = 0 Then
  Begin
    // Generate HTML file
    jsString := PickAndPlaceOutputNative(generateJS);
    GenerateHTML(jsString);
  End Else If FormatIndex = 1 Then Begin
    // Generate JSON in JavaScript file
    jsString := PickAndPlaceOutputNative(generateJS);
    DumpAsJS(jsString);
  End Else If FormatIndex = 2 Then Begin
    // Generate native JSON file
    jsonString := PickAndPlaceOutputNative(generateJS);
    DumpAsJSON(jsonString);
  End Else Begin
    // Generate generic JSON file, according to
    // https://github.com/openscopeproject/InteractiveHtmlBom/blob/master/InteractiveHtmlBom/ecad/schema/genericjsonpcbdata_v1.schema
    jsonString := PickAndPlaceOutputGeneric();
    DumpAsJSON(jsonString);
  End;
End;


{.....................................................................................................................}
{.                                                     Form Events                                                   .}
{.....................................................................................................................}


{
  OnFormShow is called when the form is shown, regardless if the form is shown
  by the OutJob via the Configure procedure, or directly opened by the Altium
  script engine.
}
procedure TMainFrm.OnFormShow(Sender: TObject);
begin
  If RunAsOutputJob Then
  Begin
    // Configure does the initialization for us
    OKBtn.Caption := 'Save Config';
  End Else Begin
    // This is our entry point if the form is opened directly.
    Initialize;
    PopulateChoiceFields();
    SetState_FromParameters('');
    SetState_Controls;
    OKBtn.Caption := 'Generate';
  End;
end;

Procedure TMainFrm.OKBtnClick(Sender: TObject);
Begin
  If RunAsOutputJob Then
  Begin
    ModalResult := mrOK;
  End Else Begin
    Generate(GetState_FromParameters());
    Close;
  End;
End;

Procedure TMainFrm.CancelBtnClick(Sender: TObject);
Begin
  If RunAsOutputJob Then
  Begin
    ModalResult := mrCancel;
  End Else Begin
    Close;
  End;
End;


{.....................................................................................................................}
{.                                                  Form Interaction                                                 .}
{.....................................................................................................................}


procedure PopulateChoiceFields;
Var
  Board: IPCB_Board; // document board object
Begin
  PopulateStaticFields();

  Board := GetBoard();
  PopulateDynamicFields(Board);
End;

procedure PopulateDynamicFields(Board: IPCB_Board);
var
  stringList: TStringList;
  argIterator: IPCB_BoardIterator;
  pcbPrimitive: IPCB_Primitive;
  pcb_primitiveparametersIntf: IPCB_PrimitiveParameters;
  argIndex: Integer;
  name: String;
begin
  stringList := TStringList.Create;
  stringList.Sorted := True;
  stringList.Duplicates := dupIgnore;
  stringList.Add('[DesignItemID]');
  stringList.Add('[Description]');
  stringList.Add('[Comment]');
  stringList.Add('[Footprint]');
  argIterator := Board.BoardIterator_Create();
  argIterator.AddFilter_ObjectSet(MkSet(eComponentObject));

  argIterator.AddFilter_IPCB_LayerSet(LayerSet.AllLayers);
  argIterator.AddFilter_Method(eProcessAll);
  pcbPrimitive := argIterator.FirstPCBObject();
  while (pcbPrimitive <> nil) do
  begin
    pcb_primitiveparametersIntf := pcbPrimitive;
    for argIndex := 0 to pcb_primitiveparametersIntf.Count() - 1 do
    begin
      name := pcb_primitiveparametersIntf.GetParameterByIndex(argIndex)
        .GetName();

      begin
        stringList.Add(name);
      end;
    end;
    pcbPrimitive := argIterator.NextPCBObject();
  end;
  Board.BoardIterator_Destroy(argIterator);

  ValueParameterCb.Items.AddStrings(stringList);
  ColumnsParametersClb.Items.AddStrings(stringList);
  GroupParametersClb.Items.AddStrings(stringList);
end;

procedure PopulateStaticFields;
Var
  Board: IPCB_Board; // document board object
Begin
  LayerFilterCb.Items.Add('Both');
  LayerFilterCb.Items.Add('Top');
  LayerFilterCb.Items.Add('Bottom');

  FormatCb.Items.Add('HTML');
  FormatCb.Items.Add('JS');
  FormatCb.Items.Add('JSON');
  FormatCb.Items.Add('JSON Generic');
End;

function GetSelectedFields: TStringList;
var
  s: TStringList;
  i: Integer;
begin
  s := TStringList.Create;

  for i := 0 to ColumnsParametersNames.Count - 1 do
  begin
    s.Add(ColumnsParametersNames[i]);
  end;
  Result := s;
end;

function GetSelectedGroupParameters: TStringList;
var
  s: TStringList;
  i: Integer;
begin
  s := TStringList.Create;

  for i := 0 to GroupParametersNames.Count - 1 do
  begin
    s.Add(GroupParametersNames[i]);
  end;
  Result := s;
end;

