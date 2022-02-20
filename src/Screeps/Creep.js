"use strict";

exports.unsafeGetAllCreepEff = function(creep){
    return function(){
        return Memory.creeps[creep];
    }
}
exports.unsafeGetCreepEff = function(creep,key){
    return function(){
        return Memory.creeps[creep][key];
    }
}
exports.unsafeSetCreepEff = function(creep,key){
    return function(val){
        return Memory.creeps[creep][key] = val;
    }
}

exports.unsafeDeleteCreepEff = function(key){
    return function(obj){
        return function(){
            delete obj.creeps[key];
        }
    }
}
