var solutions       =   ArisData.getSelectedObjDefs();
var oMethodFilter   =   ArisData.ActiveFilter();
var g_nloc          =   Context.getSelectedLanguage();
var outfile         =   null;
var cBlue =             RGB(0, 10, 128);
var defaultFont =       "Trebuchet MS";
var attrIdent =         Constants.AT_ID;
// value of variable currentRelease needs to be changed after each new release
var currentRelease =    "Prime 1.2 Release (2013-12-31)"
var attrStatus =        oMethodFilter.UserDefinedAttributeTypeNum("3f34c100-0af6-11e1-2da4-005056a93b02");
var attrAssetStatus =   oMethodFilter.UserDefinedAttributeTypeNum("caef0800-1aae-11e1-2da4-005056a93b02");
//attrPrimeRelease represents 'Prime_Release' attribue in ARIS
var attrPrimeRelease = oMethodFilter.UserDefinedAttributeTypeNum("5a1245d0-6e0e-11e1-68fc-782bcb92adba");
var attrType =          oMethodFilter.UserDefinedAttributeTypeNum("47e24710-1a90-11e1-2da4-005056a93b02");
var attrMaturity =      oMethodFilter.UserDefinedAttributeTypeNum("e4b79f90-0af5-11e1-2da4-005056a93b02");
var attr60Days =        oMethodFilter.UserDefinedAttributeTypeNum("fa0dcfe0-0af5-11e1-2da4-005056a93b02");
var attrArtifact =      oMethodFilter.UserDefinedAttributeTypeNum("47e24710-1a90-11e1-2da4-005056a93b02");

function main () {
  outfile = Context.createOutputObject(Context.getSelectedFormat(), Context.getSelectedFile());
  outfile.Init(g_nloc);
  outfile.DefineF("Heading 0", defaultFont, 11, Constants.C_WHITE, cBlue, Constants.FMT_LEFT | Constants.FMT_BOLD | Constants.FMT_TOCENTRY0,0,0,5,0,0,1.5);
  outfile.DefineF("Normal", defaultFont, 10, Constants.C_BLACK, Constants.C_TRANSPARENT, Constants.FMT_LEFT,0,0,0,0,0,0);
  outfile.BeginTable(100, Constants.C_BLACK, Constants.C_TRANSPARENT, Constants.FMT_LEFT, 0);
  outfile.TableRow();
  outfile.TableCellF(oMethodFilter.AttrTypeName(attrIdent), 20, "Heading 0");
  outfile.TableCellF(getString("TEXT_1"), 20, "Heading 0");
  outfile.TableCellF(getString("TEXT_5"), 20, "Heading 0");
  outfile.TableCellF(getString("TEXT_2"), 20, "Heading 0");
  outfile.TableCellF(getString("TEXT_6"), 20, "Heading 0");
  outfile.TableCellF(oMethodFilter.AttrTypeName(attrStatus), 20, "Heading 0");
  outfile.TableCellF(oMethodFilter.AttrTypeName(attrPrimeRelease), 20, "Heading 0");
  solutions = ArisData.sort(solutions, attrIdent, Constants.AT_NAME, g_nloc);
  for (var i in solutions) {
    //Initialize variables with 0 because solution might not be in the current release scope
    var workPackages = [];
    var workPackagesReleased = 0;
    var assets = [];
    var assetsReleased = 0;
    var assignedEPCs = solutions[i].AssignedModels(Constants.MT_EEPC);
    var valueIdent = getAttribute(solutions[i], attrIdent);
    if (valueIdent.equals(""))
      valueIdent = getAttribute(solutions[i], Constants.AT_NAME);
    
    //Only look for those solutions which belong to the current release
    if(getAttribute(solutions[i], attrPrimeRelease).equals(currentRelease)){
      var workPackages = getWorkPackages(solutions[i]);
      
      for (var j in workPackages) {
        if (getAttribute(workPackages[j], attrAssetStatus).equals("released"))
          workPackagesReleased++;
      }
      
      assets = getAssets(workPackages);
      for (var j in assets) {
        if (getAttribute(assets[j], attrAssetStatus).equals("released") )
          assetsReleased++;
      }
    }
    
    outfile.TableRow();
    outfile.TableCellF(valueIdent, 20, "Normal");
    outfile.TableCellF(workPackages.length, 20, "Normal");
    outfile.TableCellF(workPackagesReleased, 20, "Normal");
    outfile.TableCellF(assets.length, 20, "Normal");
    outfile.TableCellF(assetsReleased, 20, "Normal");
    outfile.TableCellF(getAttribute(solutions[i], attrStatus), 20, "Normal");
    outfile.TableCellF(getAttribute(solutions[i], attrPrimeRelease), 20, "Normal");
  }
  outfile.EndTable("Status", 100, defaultFont, 10, Constants.C_TRANSPARENT, Constants.C_TRANSPARENT, 0, Constants.FMT_LEFT, 0);
  outfile.BeginTable(100, Constants.C_BLACK, Constants.C_TRANSPARENT, Constants.FMT_LEFT, 0);
  outfile.TableRow();
  outfile.TableCellF(oMethodFilter.AttrTypeName(attrIdent), 20, "Heading 0");
  outfile.TableCellF(getString("TEXT_3"), 20, "Heading 0");
  outfile.TableCellF(getString("TEXT_4"), 20, "Heading 0");
  for (var i in solutions) {
    var valueIdent = getAttribute(solutions[i], attrIdent);
    if (valueIdent.equals(""))
      valueIdent = getAttribute(solutions[i], Constants.AT_NAME);
    outfile.TableRow();
    outfile.TableCellF(valueIdent, 20, "Normal");
    outfile.TableCellF(getAttribute(solutions[i], attrMaturity), 20, "Normal");
    outfile.TableCellF(getAttribute(solutions[i], attr60Days), 20, "Normal");
  }
  outfile.EndTable("Maturity", 100, defaultFont, 10, Constants.C_TRANSPARENT, Constants.C_TRANSPARENT, 0, Constants.FMT_LEFT, 0);
  outfile.WriteReport();
}

function getWorkPackages(solution) {
  var workPackages = new Array();
  var workPackagesCurrentRelease = new Array();
  
  var assignedVACDs = solution.AssignedModels(Constants.MT_VAL_ADD_CHN_DGM);
  for (var j in assignedVACDs) {
    var currWPs = assignedVACDs[j].ObjDefListFilter("Work Package", g_nloc, Constants.OT_FUNC, attrArtifact);
    workPackages = workPackages.concat(currWPs);
  }
  workPackages = ArisData.Unique(workPackages);
  
  //Extract those work packages which belong the the current release
  for (var i in workPackages){
    if (getAttribute(workPackages[i], attrPrimeRelease).equals(currentRelease))
      workPackagesCurrentRelease.push(workPackages[i]);
  }
  return workPackagesCurrentRelease;
}

function getAssets(workPackages) {
  var assignedEPCs = new Array();
  var assets = new Array();
  for (var i in workPackages) {
    var currEPCs = workPackages[i].AssignedModels(Constants.MT_EEPC);
    assignedEPCs = assignedEPCs.concat(currEPCs);
  }
  assignedEPCs = ArisData.Unique(assignedEPCs);
  for (var j in assignedEPCs) {
    var epcFuncs = assignedEPCs[j].ObjDefListFilter(Constants.OT_FUNC);
    for (var k in epcFuncs) {
      var currAssets = getConnectedObjDefs(epcFuncs[k], Constants.EDGES_IN, Constants.CT_PROV_INP_FOR, Constants.OT_INFO_CARR);
      for (var l in currAssets) {
        //Extract only assets which belong to the current release
        if (getAttribute(currAssets[l], attrType).equals("Asset") && getAttribute(currAssets[l], attrPrimeRelease).equals(currentRelease))
          assets.push(currAssets[l]);
      }
    }
  }
  assets = ArisData.Unique(assets);
  return assets;
}

function RGB(r, g, b) {
  return (new java.awt.Color(r/255.0,g/255.0,b/255.0,1)).getRGB() & 0xFFFFFF
}

function getConnectedObjOccs(oObjOcc, cxnDirection, cxnType, objType) {
  var oObjOccs = new Array();
  var cxnOccs = oObjOcc.Cxns(cxnDirection, Constants.EDGES_ALL);
  for (var i in cxnOccs) {
    if ((cxnDirection == Constants.EDGES_OUT) && (cxnOccs[i].TargetObjOcc().ObjDef().TypeNum() == objType) && (cxnOccs[i].CxnDef().TypeNum() == cxnType))
      oObjOccs.push(cxnOccs[i].TargetObjOcc());
    else if ((cxnDirection == Constants.EDGES_IN) && (cxnOccs[i].SourceObjOcc().ObjDef().TypeNum() == objType) && (cxnOccs[i].CxnDef().TypeNum() == cxnType))
      oObjOccs.push(cxnOccs[i].SourceObjOcc());
  }
  oObjOccs = ArisData.sort(oObjOccs, Constants.AT_NAME, g_nloc);
  return oObjOccs;
}

function getConnectedObjDefs(oObjDef, cxnDirection, cxnType, objType) {
  var oObjDefs = new Array();
  var cxnDefs = oObjDef.CxnListFilter(cxnDirection, cxnType);
  for(var i in cxnDefs) {
    if ((cxnDirection == Constants.EDGES_OUT) && (cxnDefs[i].TargetObjDef().TypeNum() == objType))
      oObjDefs.push(cxnDefs[i].TargetObjDef());
    else if ((cxnDirection == Constants.EDGES_IN) && (cxnDefs[i].SourceObjDef().TypeNum() == objType))
      oObjDefs.push(cxnDefs[i].SourceObjDef());
  }
  return oObjDefs;
}

function getConnectedObjDef(oObjDef, cxnDirection, cxnType, objType) {
  var cxnDefs = oObjDef.CxnListFilter(cxnDirection, cxnType);
  for(var i in cxnDefs) {
    if ((cxnDirection == Constants.EDGES_OUT) && (cxnDefs[i].TargetObjDef().TypeNum() == objType))
      return cxnDefs[i].TargetObjDef();
    else if ((cxnDirection == Constants.EDGES_IN) && (cxnDefs[i].SourceObjDef().TypeNum() == objType))
      return cxnDefs[i].SourceObjDef();
  }
  return null;
}

function getAttribute(oItem, attrNum) {
  if ((oItem != null) && (oItem.IsValid() == true)) {
    var attrValue = oItem.Attribute(attrNum, g_nloc).GetValue(true);
    return attrValue;
  }
  return "";
}

main();