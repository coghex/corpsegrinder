"use strict";

exports.createRoomPosition = function(x){
  return function(y){
    return function(roomName){
      return new RoomPosition(x, y, roomName);
    }
  }
}

