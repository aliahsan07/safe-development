  function testcase() 
  {
    var proto = {
      
    };
    Object.defineProperty(proto, "parent", {
      get : (function () 
      {
        return "parent";
      }),
      configurable : true
    });
    var Con = (function () 
    {
      
    });
    Con.prototype = proto;
    var child = new Con();
    var result = Object.getOwnPropertyNames(child);
    for(var p in result)
    {
      if (result[p] === "parent")
      {
        return false;
      }
    }
    return true;
  }
  {
    var __result1 = testcase();
    var __expect1 = true;
  }
  