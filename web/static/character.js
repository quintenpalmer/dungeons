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

    var buildSelectables = function(name, container, member) {
        var i = 1;
        container.empty();
        for (var memName in data[member]) {
            memIndex = memName;
            var rowDiv = $('<div/>', {
                class: "row"
            });

            var colDiv = $('<div/>', {
                class: 'col-sm-6'
            });

            var par = $('<p/>');

            var a = $('<a/>', {
                id: memIndex,
                href: '#',
                class: 'selectable',
                text: memName
            });

            container.append(rowDiv.append(colDiv.append(par.append(a))));
        }
    }
    buildSelectables('feat', $('#feats'), 'feats');
    buildSelectables('magicItem', $('#magicItems'), 'magicItems');
    buildSelectables('power', $('#powers'), 'powers');
}

$(document).ready(function() {
    getCharacter();
    $(document).on('click', '.selectable', function(e) {
        e.preventDefault();
        name = this.id;
        $('#selectedTitle').text(name);
        desc = character[$(this).parent().parent().parent().parent().attr('id')][name].split(' - ');
        var table = $('<ul/>');
        for (var row in desc) {
            table.append($('<li/>', { text: desc[row] }));
        }
        $('#selected').empty();
        $('#selected').append(table);
    });

});
