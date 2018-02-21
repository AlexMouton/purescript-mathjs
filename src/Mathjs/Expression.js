"use strict";

var mathjs = require("mathjs");

exports._compile = function(left) {
  return function(right) {
    return function(expr) {
      return function() {
        try {
          return right(mathjs.compile(expr))
        } catch(error) {
           return left(error)
        }
      }
    }
  }
}

function demux( undef, bool, number, string, vector, matrix, set, res ) {
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
      if( typeof res._data[0] == 'number') {
        rval = vector(res)
      } else {
        rval = matrix(res)
      }
    } else if ( res.type === 'ResultSet') {
      rval = set( res.entries.map( function(e) { return demux(undef, bool, number, string, vector, matrix, set, e) } ) )
    }
  }
  // console.log('demux', res, rtype, rval);
  return rval;
}

// The expression parser supports booleans, numbers, complex numbers, units, strings, matrices, and objects.
// Units
exports._eval =
  function( tuple ) {
    return function( undef ) {
      return function( bool ) {
        return function( number ) {
          return function( string ) {
            return function( vector ) {
              return function( matrix ) {
                return function( set ) {
                  return function( exp ) {
                    return function( scope ) {
                      return function() {
                        var sc = Object.assign({}, scope)
                        var res = exp.eval(sc)
                        var rval = demux(undef, bool, number, string, vector, matrix, set, res )
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
