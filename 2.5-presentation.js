var dispatch = {}; 
var put = function(op, type, f){
    dispatch[op + type] = f;
};
var get = function(op, type){
    return dispatch[op + type]; 
};
var type_tag = function(obj){
    return obj[0];
};
var contents = function(obj){
    return obj[1];
};
var attach_tag = function(tag, obj){
    return [tag, obj];
};

var installRectPackage = function(){
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

var apply_generic = function(op, arg){
    var type_tags = type_tag(arg);
    var proc = get(op, type_tags);
    if (proc) {
        return proc(contents(arg));
    } else {
        return "No method for these types -- APPLY-GENERIC";
    }
};

var real_part = function(z){return apply_generic('real_part', z);};
var imag_part = function(z){return apply_generic('imag_part', z);};
var magnitude = function(z){return apply_generic('magnitude', z);};
var angle = function(z){return apply_generic('angle', z);};

var make_from_real_imag = function (x, y) {
    return get('make_from_real_imag', 'rectangular')(x, y);
};

installRectPackage();

var rect1 = make_from_real_imag(2, 2);
var rect2 = make_from_real_imag(7, 0);


var add_complex = function(z1, z2) {
  return make_from_real_imag((real_part(z1) + real_part(z2)),
                             (imag_part(z1) + imag_part(z2)));
};

console.log("contesnt: " + contents(rect2));
console.log("type_tag: " + type_tag(rect2));
console.log("Real: " + real_part(rect2));
console.log("Image: " + imag_part(rect2));
console.log("mag: " + magnitude(rect2));
console.log("ang: " + angle(rect2));
console.log("sum: " + add_complex(rect1, rect2));


