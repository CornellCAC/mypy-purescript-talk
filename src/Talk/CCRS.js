"use strict";

var ccrsApiNamespace = "org.xsede.jobrunner.model.ModelApi";

var mypyPursMetaJson = {
  "$type": ccrsApiNamespace + ".SysJobMetaData",
  "shell": ["bash"],
  "containerType": {
    "$type":  ccrsApiNamespace + ".Singularity"
  },
  "containerId": [],
  "image": ["vsoch-pokemon-master-latest.simg"],
  "binds": [],
  "overlay": [],
  "user": "ccrsdemo",
  "address": [],
  "hostname": [],
  "url": window.location.href
};

exports.mypyPursMeta = CCRS.sysJobMetaData(mypyPursMetaJson);

exports.mkJobId = function () { // Effect thunk
  return CCRS.makeJobId();
};

exports.makeOneShotCommandWidg = function (node) {
  return function () { // Effect thunk
    return CCRS.makeOneShotCommandWidg(node);
  };
};

exports.updateOptCmd = function (cmdWidg) {
  return function (metaIn) {
    return function (jobId) {
      return function (cmd) {
        return function () { // Effect thunk
          return CCRS.updateOptCmd(cmdWidg, metaIn, jobId, cmd);
        };
      };
    };
  };
};

exports.makeExecFileCommandWidg = function (node) {
  return function () { // Effect thunk
    return CCRS.makeExecFileCommandWidg(node);
  };
};

exports.updateOptFileCmd = function (cmdWidg) {
  return function (metaIn) {
    return function (jobId) {
      return function (files) {
        return function (cmd) {
          return function () { // Effect thunk
            return CCRS.updateOptFileCmd(cmdWidg, metaIn, jobId, files, cmd);
          };
        };
      };
    };
  };
};

exports.makeCmdHandler = function (cmdVar) {
  return function (meta) {
    return function (jobId) {
      return function () { // Effect thunk
        return CCRS.makeCmdHandler(cmdVar, meta, jobId);
      };
    };
  };
};

exports.makeFileContents = function (fileDict) {
  return CCRS.makeFileContents(fileDict);
};