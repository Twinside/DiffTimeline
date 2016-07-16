/// <reference path="jquery.d.ts" />
/// <reference path="project.ts" />

class BreadCrumb {
    private count : number = 0;
    private current_index : number = 0;
    
    public constructor() {}

    public append_breadcrumb( name : string ) {
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

    public go_forward() {
        if (this.current_index === this.count - 1)
            return;

        this.current_index++;
        Project.state.jump_context(this.current_index);
    };

    public go_backward() {
        if (this.current_index === 0)
            return;

        this.current_index--;
        Project.state.jump_context(this.current_index);
    };

    public click_index = function( idx : number ) {
        this.current_index = idx;
        Project.state.jump_context(idx);
    }
}

var breadcrumb = new BreadCrumb();

