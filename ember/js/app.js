App = Ember.Application.create({
    LOG_TRANSITIONS: true,
});

DS.RESTAdapter.reopen({
  host: 'http://127.0.0.1:1818'
});

App.store = DS.Store.extend();

App.Commit = DS.Model.extend({
    author: DS.attr('string'),
    message: DS.attr('string'),
    timestamp: DS.attr('number'),
    parents: DS.hasMany('commit', {async:true}),
});

App.TreeDiff = DS.Model.extend({
    details: DS.attr('raw')
});


App.Router.map(function() {
  this.resource('commit', { path:'/commit/:id' }, function() { 
    this.route('treediff', { path: '/treediff/:to' });
  });

  //this.resource('remotes');
});

App.IndexRoute = Ember.Route.extend({
  model: function() { return []; }
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

App.CommitTreediffRoute = Ember.Route.extend({
  model: function(params) {
      var upper = this;
      var commit = this.modelFor('commit');
      return commit.get('parents').then(function(p) {
          return upper.store.find('tree_diff', commit.id + '/' + p.content[0].id)
      });
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


App.ArrayTransform = DS.Transform.extend({
    serialize: function(value) { return value; },
    deserialize: function(value) {
        return Ember.ArrayProxy.create({ content: Ember.A(value) });
    },  
});

App.RawTransform = DS.Transform.extend({
  deserialize: function(serialized) { return serialized; },
  serialize: function(deserialized) { return deserialized; }
});

App.DiffElemView = Ember.View.extend({
  // templateName: "diff_elem",

  didInsertElement: function() {
    var name = this.get('filename');
    var diff = this.get('diff');
    var highlighter =
        TinySyntaxHighlighter.from_filename(true, name);

    // dest_idx
    // orig_idx
    // way
    // sub
    // size
    var chunks = this.data;
    var node = document.createElement('div');
    
    for (var i = 0; i < diff.length; i++)
    {
        var chunk = diff[i];

        for (var c = 0; c < chunk.data.length; c++)
            highlighter.colorLineInNode(node, chunk.data[c]);
    }

    this.$().append(node);
  },
});
