function getCharacter(name) {
    $.post(
        '/rest/4.0/1.0/player',
        { 'name': name },
        function(data) { populateFields(data) },
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

    var buildList = function(name, container, member, selectable) {
        container.empty();
        var table = $('<table/>', {
            class: 'table table-striped'
        });
        for (var memName in data[member]) {
            memIndex = memName;

            if(selectable) {
                var td = $('<td/>', {
                    id: memIndex,
                });

                var a = $('<a/>', {
                    id: memIndex,
                    href: '#',
                    class: 'selectable',
                    text: memName
                });
                var tr = $('<tr/>');
                td.append(a);
                tr.append(td)
                table.append(tr);

            } else {
                var td1 = $('<td/>', {
                    id: memIndex,
                    text: memName
                });
                var td2 = $('<td/>', {
                    id: memIndex + "Count",
                    text: data[member][memName]
                });
                var tr = $('<tr/>');
                tr.append(td1)
                tr.append(td2)
                table.append(tr);
            }
        }
        container.append(table);
    }
    buildList('feat', $('#feats'), 'feats', true);
    buildList('power', $('#powers'), 'powers', true);
    buildList('magicItem', $('#magicItems'), 'magicItems', true);
    buildList('item', $('#items'), 'items', false);
}

$(document).ready(function() {
    getCharacter();
    $('#updateXp').submit(function(formEvent) {
        formEvent.preventDefault();
        var value = $('#newXp').val();
        if(value == "") {
            return false;
        }
        value = parseInt(value);
        value += parseInt($('#xp').val());
        console.log('hi there');
        console.log(formEvent);
        $.post(
            '/rest/4.0/1.0/update',
            {'key': 'xp', 'value': value },
            function (data) {
                console.log(data);
                getCharacter(data);
            },
            'json');
    });

    $('#updateGold').submit(function(formEvent) {
        formEvent.preventDefault();
        var value = $('#newGold').val();
        console.log(value);
        if(value == "") {
            console.log("no value in #newGold");
            return false;
        }
        value = parseInt(value);
        var updateAmount = $('#GoldCount').text();
        console.log(updateAmount);
        if(updateAmount == "") {
            console.log("no value in #GoldCount");
            return false;
        }
        value += parseInt(updateAmount);
        console.log('hi there');
        console.log(formEvent);
        $.post(
            '/rest/4.0/1.0/update',
            {'key': 'Gold', 'value': value },
            function (data) {
                console.log(data);
                getCharacter(data);
            },
            'json');
    });

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

});
