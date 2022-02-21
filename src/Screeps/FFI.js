"use strict";

// module Screeps.FFI

exports.unsafeField = function(key){
    return function(obj){
        return obj[key];
    }
}
exports.unsafeGetFieldEff = function(key){
    return function(obj){
        return function(){
            return obj[key];
        }
    }
}
exports.unsafeSetFieldEff = function(key){
    return function(obj){
        return function(val){
            return function(){
                obj[key] = val;
            }
        }
    }
}
exports.unsafeSetFieldKeyEff = function(key){
    return function(obj){
        return function(o){
            return function(val){
                return function(){
                    obj[o][key] = val;
                }
            }
        }
    }
}
exports.unsafeDeleteFieldEff = function(key){
  return function(obj){
      return function(){
        delete obj[key];
      }
  }
}
exports.unsafeDeleteFieldKeyEff = function(o,key){
  return function(obj){
      return function(){
        delete obj[o][key];
      }
  }
}
exports.unsafeClearEff = function() {
    return function(obj){
        return function() {
            for (const key in obj) {
                delete obj[key];
            }
        }
    }
}
exports.runThisEffFn0 = function(key){
    return function(self){
        return function(){
            return self[key]();
        }
    }
}
exports.runThisEffFn1 = function(key){
    return function(self){
        return function(a){
            return function(){
                return self[key](a);
            }
        }
    }
}
exports.runThisEffFn2 = function(key){
  return function(self){
    return function(a){
      return function(b){
        return function(){
          return self[key](a, b);
        }
      }
    }
  }
}
exports.runThisEffFn3 = function(key){
  return function(self){
    return function(a){
      return function(b){
        return function(c){
          return function(){
            return self[key](a, b, c);
          }
        }
      }
    }
  }
}
exports.runThisEffFn4 = function(key){
  return function(self){
    return function(a){
      return function(b){
        return function(c){
          return function(d){
            return function(){
              return self[key](a, b, c, d);
            }
          }
        }
      }
    }
  }
}
exports.runThisFn0 = function(key){
  return function(self){
    return self[key]();
  }
}
exports.runThisFn1 = function(key){
  return function(self){
    return function(a){
      return self[key](a);
    }
  }
}
exports.runThisFn2 = function(key){
  return function(self){
    return function(a){
      return function(b){
        return self[key](a,b);
      }
    }
  }
}
exports.runThisFn3 = function(key){
  return function(self){
    return function(a){
      return function(b){
        return function(c){
          return self[key](a,b,c);
        }
      }
    }
  }
}
exports.null = null;
exports.undefined = undefined
exports.notNullOrUndefined = function(x){
    return x;
}
exports.isNull = function(x){
    return x === null;
}
exports.isUndefined = function(x){
    return x === undefined;
}
exports.toMaybeImpl = function (val, nothing, just){
    if(val === null || val === undefined){
        return nothing;
    } else {
        return just(val);
    }
}
exports.selectMaybesImpl = function(isJust){
    return function(fromJust){
        return function(obj){
            var newObj = {};
            for(var key in obj){
                if(!obj.hasOwnProperty(key)){
                    continue;
                }
                if(isJust(obj[key])){
                    newObj[key] = fromJust(obj[key]);
                }
            }
            return newObj;
        }
    }
}
