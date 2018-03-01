"use strict";

var mathjs = require("mathjs");

exports._compile = function(expr) {
  return function() {
    return mathjs.compile(expr);
  }
}

function demux( bool, number, string, vector, matrix, pair, obj, set, undef, res ) {
  var rval = undef
  var rtype = typeof res
  if( rtype === 'boolean' ) {
    rval = bool(res)
  } else if( rtype === 'number') {
    rval = number(res)
  } else if ( rtype === 'string' ) {
    rval = string(res)
  } else if ( rtype === 'object') {
    if( res.type === 'DenseMatrix' ) {
      if( res._size.length == 1 ) {
        rval = vector(res)
      } else if ( res._size.length == 2 ) {
        rval = matrix(res)
      }
    } else if ( res.type === 'ResultSet') {
      rval = set( res.entries.map( function(e) { return demux(bool, number, string, vector, matrix, pair, obj, set, undef, e) } ) )
    } else if ( !res.type ) {
      const pairs = Object.keys(res).map( function(key) {
          var val = demux(bool, number, string, vector, matrix, pair, obj, set, undef, res[key])
          return pair(key)(val)
        })
      rval = obj(pairs)
    }
  }
  // console.log('demux', res, rtype, rval);
  return rval;
}

// The expression parser supports booleans, numbers, complex numbers, units, strings, matrices, and objects.
// Units
exports._eval =
  function( tuple ) {
    return function( bool ) {
      return function( number ) {
        return function( string ) {
          return function( vector ) {
            return function( matrix ) {
              return function( pair ) {
                return function( obj ) {
                  return function( set ) {
                    return function( undef ) {
                      return function( exp ) {
                        return function( scope ) {
                          return function() {
                            var sc = Object.assign({}, scope)
                            var res = exp.eval(sc)
                            var rval = demux( bool, number, string, vector, matrix, pair, obj, set, undef, res )
                            // console.log('eval', rval, scope )
                            return tuple(rval)(sc)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
