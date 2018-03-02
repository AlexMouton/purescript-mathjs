"use strict";

var mathjs = require("mathjs");

exports._compile = function(expr) {
  return function() {
    return mathjs.compile(expr);
  }
}

function demux( constructors, res ) {
  var rval = constructors.undef
  var rtype = typeof res
  if( rtype === 'boolean' ) {
    rval = constructors.bool(res)
  } else if( rtype === 'number') {
    rval = constructors.number(res)
  } else if ( rtype === 'string' ) {
    rval = constructors.string(res)
  } else if ( rtype === 'object') {
    if( res.type === 'DenseMatrix' ) {
      if( res._size.length == 1 ) {
        rval = constructors.vector(res)
      } else if ( res._size.length == 2 ) {
        rval = constructors.matrix(res)
      }
    } else if ( res.type === 'ResultSet') {
      rval = constructors.set( res.entries.map( function(e) { return demux( constructors, e ) } ) )
    } else if( res.type === 'BigNumber') {
      rval = constructors.bignumber(res)
    } else if( res.type === 'Complex') {
      rval = constructors.complex(res)
    } else if ( !res.type ) {
      const pairs = Object.keys(res).map( function(key) {
          var val = demux( constructors, res[key] )
          return constructors.pair(key)(val)
        })
      rval = constructors.obj(pairs)
    }
  }
  // console.log('demux', res, rtype, rval);
  return rval;
}

// The expression parser supports booleans, numbers, complex numbers, units, strings, matrices, and objects.
// Units
exports._eval =
  function( eo ) {
    return function( constructors ) {
      return function( exp ) {
        return function( scope ) {
          return function() {
            var sc = Object.assign({}, scope)
            var res = exp.eval(sc)
            var rval = demux( constructors, res )
            // console.log('eval', rval, scope )
            return eo(rval)(sc)
          }
        }
      }
    }
  }
