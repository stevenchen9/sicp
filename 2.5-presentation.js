var dispatch = {}; 
var put = function(op, type, f){
    dispatch[op][type] = f;
};
var get = function(op, type){
    return dispatch[op][type]; 
};
var attach_tag = function(tag, obj){
    obj.unshift(tag);
    return obj;
};

var installPolarPackage = function(){
  var real_part = function(z) { return z[0];};
  var imag_part = function(z) { return z[1];};
  var make_from_real_imag = function(x, y) { return [x, y];};
  var magnitude = function(z) {
      return Math.sqrt(((real_part(z))^2) + ((imag_part(z))^2));
  };
  var angle = function(z) {
    return Math.atan(imag_part(z), real_part(z));
  };
  var make_from_mag_ang = function(r, a) {
    return [(r * Math.cos(a)), (r * Math.sin(a))];
  };
  var tag = function(x) { return attach_tag('rectangular', x);};

  put('real_part', ['rectangular'], real_part);
  put('imag_part', ['rectangular'], imag_part);
  put('magnitude', ['rectangular'], magnitude);
  put('angle', ['rectangular'], angle);
  put('make_from_real_imag', 'rectangular',
       function(x, y){ return tag(make_from_real_imag(x, y));});
  put('make_from_mag_ang', 'rectangular',
       function(x, y){ return tag(make_from_mag_ang(x, y));});
}; 

console.log("tadf");
