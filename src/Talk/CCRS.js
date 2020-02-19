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

exports.makeOneShotCommand = function (node) {
  return function () { // Effect thunk
    return CCRS.makeOneShotCommand(node);
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
