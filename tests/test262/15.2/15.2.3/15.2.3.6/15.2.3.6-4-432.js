//   TODO getter/setter
//   function testcase() 
//   {
//     var obj = {
//       
//     };
//     Object.defineProperty(obj, "prop", {
//       get : undefined,
//       set : undefined,
//       enumerable : true,
//       configurable : false
//     });
//     var propertyDefineCorrect = obj.hasOwnProperty("prop");
//     var desc = Object.getOwnPropertyDescriptor(obj, "prop");
//     return propertyDefineCorrect && typeof desc.set === "undefined";
//   }
//   {
//     var __result1 = testcase();
//     var __expect1 = true;
//   }
//   