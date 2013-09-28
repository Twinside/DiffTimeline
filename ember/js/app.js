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

  /*
  setupController: function(controller, playlist) {
    controller.set('model', playlist.get('commit'));
  }, // */
  

  actions: {
      error: function(reason) {
          alert("COMMIT: " + reason.toString());
      },

      expand: function(to) {
          // todo =)
          // this.pushObject(st);
      }
  }
});

/*
App.CommitController = Ember.ArrayController.extend({
});
// */

App.CommitTreediffRoute = Ember.Route.extend({
  model: function(params) {
      var upper = this;
      var commit = this.modelFor('commit');
      return commit.get('parents').then(function(p) {
          return upper.store.find('tree_diff', commit.id + '/' + p.content[0].id)
      });
  },

  renderTemplate: function() {
    this.render({outlet: 'detail'});
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

App.BranchTransform = DS.Transform.extend({
  deserialize: function(serialized) {
  }
});

var commit_renderer = function(filename, diff) {
    var hl = TinySyntaxHighlighter.from_filename(false, filename);

    /** @type {jQuery} */
    var rez_node = document.createElement('table');
    var row = document.createElement('tr');
    rez_node.appendChild(row);
    var numberCell1 = document.createElement('td');
    var numberCell2 = document.createElement('td');
    var codeCell = document.createElement('td');

    row.appendChild(numberCell1);
    row.appendChild(codeCell);
    row.appendChild(numberCell2);

    /** @type {Element} */
    var code_node = document.createElement('pre');
    code_node.setAttribute('class', 'syntax_highlighted');
    codeCell.appendChild(code_node);

    /** @type {Element} */
    var number_node = document.createElement('pre');
    number_node.setAttribute('class', 'line_number_column');
    numberCell1.appendChild(number_node);

    var number_node2 = document.createElement('pre');
    number_node2.setAttribute('class', 'line_number_column');
    numberCell2.appendChild(number_node2);

    var curr_diff;
    var acc = ""
    var diff_node;
    var prev_end = -1;

    var create_number_node = function(n) {
        var node = document.createElement('span');
        node.setAttribute('class', 'syntax_line_number');
        node.appendChild(document.createTextNode(n.toString() + "\n"));
        return node;
    };

	/** @type {function(string) : Element} */
    var div_node = function(kind) {
        var node = document.createElement('div');
        node.setAttribute('class', kind);
        return node;
    };

    for ( var i = 0; i < diff.length; i++ )
    {
        var has_sub = false;

        curr_diff = diff[i];
		has_sub = (curr_diff.hasOwnProperty('sub') &&
				   curr_diff.sub.length > 0);

        if (curr_diff.way == '+') {
            diff_node = div_node("diff_addition");
            hl.set_current_line_number(curr_diff.dest_idx + 1);
        }
        else if (curr_diff.way == '-') {
            diff_node = div_node("diff_deletion");
            hl.set_current_line_number(curr_diff.dest_idx + 1);
        }
        else {
            diff_node = div_node("diff_context");
            hl.set_current_line_number(curr_diff.dest_idx + 1);

            if (prev_end != curr_diff.dest_idx)
                hl.reset_context();
        }

        prev_end = curr_diff.dest_idx + curr_diff.data.length;

        for ( var l = 0; l < curr_diff.data.length; l++ )
        {
            if (has_sub) {
                hl.setPositionHighlight(curr_diff.sub[l]);
            }

            var lineNodes = hl.colorLine(curr_diff.data[l] + "\n");
            if (has_sub) {
                hl.setPositionHighlight([]);
            }

            if (curr_diff.way == '=')
            {
                number_node.appendChild(create_number_node(curr_diff.orig_idx + l));
                number_node2.appendChild(create_number_node(curr_diff.dest_idx + l));
            }
            else if (curr_diff.way == '+')
            {
                number_node.appendChild(document.createTextNode('\n'));
                number_node2.appendChild(create_number_node(curr_diff.dest_idx + l));
            }
            else
            {
                number_node.appendChild(create_number_node(curr_diff.orig_idx + l));
                number_node2.appendChild(document.createTextNode('\n'));
            }


            for ( var node = 0; node < lineNodes.length; node++ )
                diff_node.appendChild(lineNodes[node]);
        }

        code_node.appendChild(diff_node);

        if (i < diff.length - 1 &&
            curr_diff.dest_idx + curr_diff.size < diff[i + 1].dest_idx )
        {
            number_node.appendChild(document.createTextNode("\n...\n\n"));
            number_node2.appendChild(document.createTextNode("\n...\n\n"));
            code_node.appendChild(document.createTextNode("\n...\n\n"));
        }

    }

    return rez_node;
};

App.DiffElemView = Ember.View.extend({
  didInsertElement: function() {
    var name = this.get('filename');
    var diff = this.get('diff');

    this.$().append(commit_renderer(name, diff));
  },
});
