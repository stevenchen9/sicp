




















var dispatch = {}; 
var dispatchC = {}; 
var put_coercion = function(t1, t2, f){
    dispatchC[t1 + t2] = f;
};
var get_coercion = function(t1, t2){
    return dispatchC[t1 + t2]; 
};
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

var apply_generic = function(op, args){
    var type_tags = args.map(type_tag);
    var proc = get(op, type_tags);
    if (proc) {
        return proc.apply(this, args.map(contents));
    } else {
        return "No method for these types -- APPLY-GENERIC" + type_tags;
    }
};
apply_generic = function(op, args){
    var type_tags = args.map(type_tag);
    var proc = get(op, type_tags);
    if (proc) {
        return proc.apply(this, args.map(contents));
    } else {
        var t1 = type_tags[0];
        var t2 = type_tags[1];
        var a1 = args[0];
        var a2 = args[1];
        var t1TOt2 = get_coercion(t1, t2);
        var t2TOt1 = get_coercion(t2, t1);

        if (t1TOt2) {
            console.log("Casting " + t1 + " to " + t2);
            return apply_generic(op, [t1TOt2(a1), a2]);
        } else if(t2TOt1) {
            console.log("Casting " + t2 + " to " + t1);
            return apply_generic(op, [a1, t2TOt1(a2)]);
        } else {
            return "No method for these types -- APPLY-GENERIC" + type_tags;
        }
    }
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

var install_scheme_number_package = function() {
  var integer_rational = function (i) {return make_rational(i, 1);};
  var tag = function(x) { return attach_tag('scheme_number', x);};
  put('add', ['scheme_number','scheme_number'],
       function(x,y){ return tag(x + y);});
  put('sub', ['scheme_number','scheme_number'],
       function(x,y){return tag(x + y);});
  put('mul', ['scheme_number','scheme_number'],
       function(x,y){return tag(x * y);});
  put('div', ['scheme_number','scheme_number'],
       function(x,y){return tag(x / y);});
  put('make', 'scheme_number',
       function(x) { return tag(x);});

  put_coercion('scheme_number', 'rational', integer_rational);
};


var install_rational_package = function() {
  // internal procedures
  var numer = function(x) { return x[0];};
  var denom = function(x) { return x[1];};
  var make_rat = function(n, d) {return [n, d];};
  var add_rat = function(x, y) {
    return make_rat((numer(x) * denom(y)) 
                    + (numer(y) * denom(x)),
                    (denom(x) * denom(y)));};
  var sub_rat = function(x, y) {
    return make_rat((numer(x) * denom(y)) 
                    - (numer(y) * denom(x)),
                    (denom(x) * denom(y)));};
  var mul_rat = function(x, y) {
    return make_rat((numer(x) * numer(y)),
                    (denom(x) * denom(y)));};
  var div_rat = function(x, y) {
    return make_rat((numer(x) * denom(y)),
                    (denom(x) * numer(y)));};

  //interface to rest of the system
  var tag = function(x) { return attach_tag('rational', x);};
  put('add', ['rational', 'rational'],
       function(x,y){ return tag(add_rat(x, y));});
  put('sub', ['rational','rational'],
       function(x,y){ return tag(sub_rat(x, y)); });
  put('mul', ['rational','rational'],
       function(x,y){ return  tag(mul_rat(x, y)); });
  put('div', ['rational','rational'],
       function(x,y){ return  tag(div_rat(x, y)); });

  put('make', 'rational',
       function(n,d){ return tag(make_rat(n, d)); });
};


var install_complex_package = function() {
  // imported procedures from rectangular and polar packages
  var make_from_real_imag = function(x, y) {
    return get('make_from_real_imag', 'rectangular')(x, y); };
  var make_from_mag_ang = function(r, a) {
    return get('make_from_mag_ang', 'polar')(r, a); };

  // internal procedures
  var add_complex = function(z1, z2) {
    return make_from_real_imag(real_part(z1) + real_part(z2),
                               imag_part(z1), imag_part(z2));};
  var sub_complex = function(z1, z2) {
    return make_from_real_imag(real_part(z1) - real_part(z2),
                               imag_part(z1) - imag_part(z2));};
  var mul_complex = function(z1, z2) {
    return make_from_mag_ang(magnitude(z1) * magnitude(z2),
                             angle(z1) + angle(z2));};
  var div_complex = function(z1, z2) {
    return make_from_mag_ang(magnitude(z1) / magnitude(z2),
                             angle(z1) - angle(z2));};

  // interface to rest of the system
  var tag = function(z) { return attach_tag('complex', z); };
  put('add', ['complex','complex'],
       function(z1,z2){ return tag(add_complex (z1, z2)); });
  put('sub', ['complex','complex'],
       function(z1,z2){ return tag(sub_complex (z1, z2)); });
  put('mul', ['complex','complex'],
       function(z1,z2){ return tag(mul_complex (z1, z2)); });
  put('div', ['complex','complex'],
       function(z1,z2){ return tag(div_complex (z1, z2)); });
  put('make_from_real_imag', 'complex',
       function(x,y){ return tag(make_from_real_imag(x, y));});
  put('make_from_mag_ang', 'complex',
       function(r,a){ return tag(make_from_mag_ang(r, a));});
};

var real_part = function(z){return apply_generic('real_part', [z]);};
var imag_part = function(z){return apply_generic('imag_part', [z]);};
var magnitude = function(z){return apply_generic('magnitude', [z]);};
var angle = function(z){return apply_generic('angle', [z]);};
var add = function(x, y) { return apply_generic('add', [x, y]); };
var sub = function(x, y) { return apply_generic('sub', [x, y]);};
var mul = function(x, y) { return apply_generic('mul', [x, y]);};
var div = function(x, y) { return apply_generic('div', [x, y]);};

var make_scheme_number = function(n) {
  return get('make', 'scheme_number')(n);
};
var make_from_real_imag = function (x, y) {
    return get('make_from_real_imag', 'rectangular')(x, y);
};
var make_rational = function(n, d) {
  return get('make', 'rational')(n, d); 
};
var make_complex_from_real_imag = function(x, y) {
  return get('make_from_real_imag', 'complex')(x, y); 
};
var make_complex_from_mag_ang = function(r, a) {
  return get('make_from_mag_ang', 'complex')(r, a); 
};


installRectPackage();
install_complex_package();
install_rational_package();
install_scheme_number_package();

var rect1 = make_complex_from_real_imag(2, 2);
var rect2 = make_complex_from_real_imag(7, 0);

// console.log("rect1: " + rect1);
// console.log("contents rect1: " + contents(rect1));
// console.log("type_tag rect1: " + type_tag(rect1));
// console.log("contents rect2: " + contents(rect2));
// console.log("type_tag rect2: " + type_tag(rect2));
// console.log("sum: " + add(rect1, rect2));


var r1 = make_rational(4,2);
var n1 = make_scheme_number(2);

console.log("sum: " + add(r1, n1));


add :: 'a -> 'a -> 'a














