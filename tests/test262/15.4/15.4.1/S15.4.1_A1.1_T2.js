  Array.prototype.toString = Object.prototype.toString;
  var x = Array();
  {
    var __result1 = x.toString() !== "[object " + "Array" + "]";
    var __expect1 = false;
  }
  Array.prototype.toString = Object.prototype.toString;
  var x = Array(0, 1, 2);
  {
    var __result2 = x.toString() !== "[object " + "Array" + "]";
    var __expect2 = false;
  }
  