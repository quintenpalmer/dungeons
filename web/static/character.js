$(document).ready(function() {
    getCharacter();
    registerUpdaters();
    registerSelectables();
});

function registerUpdaters() {
    $('#updateXp').submit(function(formEvent) {
        formEvent.preventDefault();
        var value = $('#newXp').val();
        if(value == "") {
            return false;
        }
        value = parseInt(value);
        value += parseInt($('#xp').val());
        $.post(
            '/rest/4.0/1.0/update',
            {'key': 'xp', 'value': value },
            function (data) {
                getCharacter(data);
            },
            'json');
    });
    registerGenericUpdater('Gold');
    registerItemUpdater();
}

function registerItemUpdater() {
    $('#updateItem').submit(getBasicFormEventHandler());
}

function getBasicFormEventHandler() {
    return function(formEvent) {
        formEvent.preventDefault();
        var name = $('#newItem').val();
        if(!name) {
            console.log("no value in #newItem");
            return false;
        }
        var value = '1';
        postUpdate(name, value);
    };
}

function getIncreasingFormEventHandler(name) {
    return function(formEvent) {
        formEvent.preventDefault();
        value = $('#new' + name).val();
        if(!value) {
            console.log("no value in #new" + name);
            return false;
        }
        value = parseInt(value);
        var updateAmount = $('#' + name + 'Count').text();
        if(!updateAmount) {
            console.log("no value in #" + name + "Count");
            return false;
        }
        value += parseInt(updateAmount);
        postUpdate(name, value);
    };
}

function registerGenericUpdater(name) {
    $('#update' + name).submit(getIncreasingFormEventHandler(name));
}

function registerSelectables() {
    $(document).on('click', '.selectable', function(e) {
        e.preventDefault();
        name = this.id;
        $('#selectedTitle').text(name);
        desc = character[$(this).parent().parent().parent().parent().parent().attr('id')][name].split(' - ');
        var table = $('<table/>', { class: 'table table-striped' });
        for (var row in desc) {
            table.append($('<tr/>').append($('<td/>', { text: desc[row] })));
        }
        $('#selected').empty();
        $('#selected').append(table);
    });
    $(document).on('click', '.updateable', function(formEvent) {
        formEvent.preventDefault();
        var name = this.id;
        var pieces = this.href.split('/');
        var amount = pieces[pieces.length - 1];

        value = parseInt(amount);
        var updateAmount = $('#' + name + 'Count').text();
        if(!updateAmount) {
            console.log("no value in #" + name + "Count");
            return false;
        }
        value += parseInt(updateAmount);

        postUpdate(this.id, value);
    });
}

function getCharacter(name) {
    $.post(
        '/rest/4.0/1.0/player',
        { 'name': name },
        function(data) { populateFields(data) },
        'json');
}

function postUpdate(name, value) {
    $.post(
        '/rest/4.0/1.0/update',
        {'key': name, 'value': value },
        function (data) {
            getCharacter(data);
        },
        'json');
}

function populateFields(data) {
    character = data;
    $('#name').val(data.name);
    $('#race').val(data.race);
    $('#armor').val(data.armor);
    $('#weapons').val(data.weapons);
    $('#class_').val(data.class_);
    $('#level').val(data.level);
    $('#xp').val(data.xp);
    $('#initiative').val(data.initiative);
    for (var abilName in data.abilityScores) {
        $('#abilScore' + abilName).val(data.abilityScores[abilName]);
        $('#abilMod' + abilName).val(data.abilityMods[abilName]);
        $('#abilModPlus' + abilName).val(data.abilityModsPlus[abilName]);
    }
    $('#health').val(data.hitPoints);
    $('#bloodied').val(data.bloodied);
    $('#surgeValue').val(data.surgeValue);
    $('#surgesPerDay').val(data.surgesPerDay);
    for (var skillName in data.skills) {
        $('#' + skillName).val(data.skills[skillName]);
    }
    for (var defName in data.defenses) {
        $('#' + defName).val(data.defenses[defName]);
    }
    $('#speed').val(data.speed);
    $('#passiveInsight').val(data.passiveInsight);
    $('#passivePerception').val(data.passivePerception);

    var buildList = function(name, selectable, updateable) {
        var container = $('#' + name + 's');
        var member = name + 's';

        container.empty();
        var table = $('<table/>', {
            class: 'table table-striped'
        });
        for(var memName in data[member]) {
            var tr = $('<tr/>');
            if(selectable) {
                var td = $('<td/>');
                var a = $('<a/>', {
                    id: memName,
                    href: '#',
                    class: 'selectable',
                    text: memName
                });
                td.append(a);
                tr.append(td)

            } else {
                var td1 = $('<td/>', {
                    text: memName
                });
                var td2 = $('<td/>', {
                    id: memName + "Count",
                    text: data[member][memName]
                });
                if(updateable) {
                    var a3 = $('<a/>', {
                        id: memName,
                        href: '-1',
                        class: 'updateable',
                        text: "-1"
                    });
                    var td3 = $('<td/>');
                    td3.append(a3);
                    var a4 = $('<a/>', {
                        id: memName,
                        href: '1',
                        class: 'updateable',
                        text: "+1"
                    });
                    var td4 = $('<td/>', {
                        id: memName,
                    });
                    td4.append(a4);

                    tr.append(td1);
                    tr.append(td3);
                    tr.append(td2);
                    tr.append(td4);
                } else {
                    tr.append(td1);
                    tr.append(td2);
                }
            }
            table.append(tr);
        }
        container.append(table);
    }
    buildList('feat', true, false);
    buildList('power', true, false);
    buildList('magicItem', true, false);
    buildList('item', false, true);
}
