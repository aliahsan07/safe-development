  x = 1;
  x &= undefined;
  {
    var __result1 = x !== 0;
    var __expect1 = false;
  }
  x = undefined;
  x &= 1;
  {
    var __result2 = x !== 0;
    var __expect2 = false;
  }
  x = new Number(1);
  x &= undefined;
  {
    var __result3 = x !== 0;
    var __expect3 = false;
  }
  x = undefined;
  x &= new Number(1);
  {
    var __result4 = x !== 0;
    var __expect4 = false;
  }
  