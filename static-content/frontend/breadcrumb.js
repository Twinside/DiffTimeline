var BreadCrumb = function() {
    /** @type {number} */
    var count = 0;

    /** @type {number} */
    var current_index = 0;
};

/** @type {function(string) : void} */
BreadCrumb.prototype.append_breadcrumb = function( name ) {
    if (this.current_index < this.count - 1)
    {
        $('#breadcrumb > span').slice(this.current_index + 1).remove();
        this.count = this.current_index + 1;
    }

    /** @type {jQuery} */
    var elem = ich.breadcrumbelem({name:name, id:this.count});
    $('#breadcrumb').append(elem);
    this.current_index = this.count++;
};

BreadCrumb.prototype.go_forward = function() {
    if (this.current_index === this.count - 1)
        return;

    this.current_index++;
    Project.state.jump_context(this.current_index);
};

BreadCrumb.prototype.go_backward = function() {
    if (this.current_index === 0)
        return;

    this.current_index--;
    Project.state.jump_context(this.current_index);
};

/** @type {function(number) : void} */
BreadCrumb.prototype.click_index = function( idx ) {
    this.current_index = idx;
    Project.state.jump_context(idx);
};

var breadcrumb = new BreadCrumb();

