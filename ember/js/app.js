App = Ember.Application.create({
    LOG_TRANSITIONS: true,
});

App.store = DS.Store.extend();

App.CommitDetail = DS.Model.extend({
});

App.Commit = DS.Model.extend({
    author: DS.attr('string'),
    message: DS.attr('string'),
    timestamp: DS.attr('number'),
    parents: DS.hasMany('commit', {async:true}),
});

App.Detail = DS.Model.extend({
    bidule: DS.attr('number')
});

App.CommitDiff = DS.Model.extend({
    bidule: DS.attr('number')
});


/*
App.Branch = DS.Model.extend({
    name: DS.attr('string'),
    ref: DS.attr('commit', {async:true})
});

App.Remote = DS.Model.extend({
    branches: DS.hasMany('branch'
                        //, { embedded: 'always' }
                        )
});

App.RemotesSerializer = DS.RESTSerializer.extend({
  extractSingle: function(store, type, payload, id, requestType) {
    var branches = payload.remote.branches;
    var branchesIds = comments.mapProperty('id');

    payload.branches = branches;
    payload.remote.branches = branchesIds;

    return this._super.apply(this, arguments);
  }
});

App.Adapter.map('App.Remote', {
  branches: { embedded: 'always' }
});
// */

/*
DS.RESTAdapter.registerTransform('remote', {
  serialize: function(value) {
    return [value.get('x'), value.get('y')];
  },
  deserialize: function(value) {
    for ( var i = 0; i < value.branch[]; i++ )
    {
        //
    }
    return Ember.create({ x: value[0], y: value[1] });
  }
});
 */

DS.RESTAdapter.reopen({
  host: 'http://127.0.0.1:1818'
});

App.Router.map(function() {
  // put your routes here
  this.resource('commit', { path:'/commit/:id' }, function() { 
    this.route('diff', { path: '/diff/:from' });
  });
  //this.resource('diff_commit', { path:'/diff_commit/:base/:result' } );

  //this.resource('remotes');
});

App.IndexRoute = Ember.Route.extend({
  model: function() {
    return ['c'];
  }
});


App.CommitRoute = Ember.Route.extend({
  model: function(params) {
      return this.store.find('commit', params.id);
  },

  actions: {
      error: function(reason) {
          alert("COMMIT: " + reason.toString());
      }
  }
});

App.CommitDiffRoute = Ember.Route.extend({
  model: function(params) {
      return this.store.find('commit.diff', params.result);
  },

  actions: {
      error: function(reason) {
          alert("DETAIL: " + reason.toString());
      }
  }
});

/*
DS.RESTAdapter.registerTransform("timestamp", {
  deserialize: function(serialized) {
      var d = new Date();
      d.setTime( serialized * 1000 );
      return d.toLocaleDateString() + " " + d.toLocaleTimeString();
  },

  serialize: function(deserialized) { return deserialized; }
});
// */

