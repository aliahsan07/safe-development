  x = - Infinity;
  y = new Array();
  y[0] = 0.000000000000001;
  y[1] = 1;
  y[2] = 1.7976931348623157e308;
  ynum = 3;
  for (i = 0;i < ynum;i++)
  {
    this["__result" + i] = ! (Math.atan2(y[i], x) === Math.PI);
	this["__expect" + i] = false;
  }
  
