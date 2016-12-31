  function testcase() 
  {
    var obj = {
      
    };
    Object.defineProperty(obj, "prop", {
      value : 2010,
      writable : true,
      enumerable : true,
      configurable : false
    });
    var propertyDefineCorrect = obj.hasOwnProperty("prop");
    var desc = Object.getOwnPropertyDescriptor(obj, "prop");
    for(var p in obj)
    {
      if (p === "prop")
      {
        return propertyDefineCorrect && desc.enumerable === true;
      }
    }
    return false;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  