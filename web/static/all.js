$(document).ready(function() {
    getCharacters();
});

function getCharacters() {
    $.post(
        '/rest/4.0/1.0/all',
        {},
        populateCharacters,
        'json');
}

function populateCharacters(data) {
    console.log(data);
    var container = $('#characters');

    container.empty();
    var table = $('<table/>', {
        class: 'table table-striped'
    });
    for(var name in data.data) {
        name = data.data[name];
        var tr = $('<tr/>');
        var td = $('<td/>');
        var a = $('<a/>', {
            href: '/dnd/4.0/character/?name=' + name,
            text: name
        });
        td.append(a);
        tr.append(td)
        table.append(tr);
    }
    container.append(table);
}
