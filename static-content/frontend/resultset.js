/** @interface */
var ResultSet = function() {};

ResultSet.prototype.create_all_dom = function() {};
ResultSet.prototype.render_all = function() {};

ResultSet.prototype.send_message = function( msg ) {};
/** @param {number} id */
ResultSet.prototype.fetch_previous = function( id ) {};

/** @typedef ({way: Project.DiffChar, orig_idx: number, dest_idx: number, size: number}) */
var DiffInfo = {};

/** @typedef({way: Project.DiffChar, beg: number, end: number}) */
var DiffRange = {};
