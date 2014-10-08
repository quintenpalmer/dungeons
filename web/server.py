import json
import socket

from flask import (Flask, request, jsonify, render_template)


app = Flask(__name__)


@app.route('/', methods=['GET'])
def home():
    return render_template("welcome.html")


@app.route('/dnd/4.0/character/', methods=['GET'])
def get_character():
    data = get_character_from_server('prompt')
    return render_template("character-4.html", **data)


def get_character_from_server(name):
    HOST = 'localhost'
    PORT = 5269
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((HOST, PORT))

    attrs = {}
    for attr in ['name', 'level', 'speed', 'initiative', 'health']:
        attrs.update(json.loads(request_attribute(attr, s)))

    for abil in ['Str', 'Con', 'Dex', 'Int', 'Wis', 'Cha']:
        response = json.loads(request_one_of('abilityScore', abil, s))
        attrs.update({'abilityScore' + abil: response['abilityScore']})

    for abil in ['Str', 'Con', 'Dex', 'Int', 'Wis', 'Cha']:
        response = json.loads(request_one_of('abilityMod', abil, s))
        attrs.update({'abilityMod' + abil: response['abilityMod']})

    for abil in ['Str', 'Con', 'Dex', 'Int', 'Wis', 'Cha']:
        response = json.loads(request_one_of('abilityModPlus', abil, s))
        attrs.update({'abilityModPlus' + abil: response['abilityModPlus']})

    for skill in [
            "acrobatics",
            "arcana",
            "athletics",
            "bluff",
            "diplomacy",
            "dungeoneering",
            "endurance",
            "heal",
            "history",
            "insight",
            "intimidate",
            "nature",
            "perception",
            "religion",
            "stealth",
            "streetwise",
            "thievery"]:
        response = json.loads(request_one_of('skill', skill.title(), s))
        attrs.update({skill: response['skill']})

    for defense in [
            "ac",
            "fort",
            "ref",
            "will"]:
        response = json.loads(request_one_of('defense', defense.title(), s))
        attrs.update({defense: response['defense']})

    s.close()
    return attrs


def request_one_of(type_, name, s):
    return send_request(type_ + ':' + name, s)

def request_attribute(attr, s):
    return send_request(attr, s)


def send_request(name, s):
    s.sendall(name + '\n')
    print name
    data = s.recv(1024)
    print data
    return data

if __name__ == '__main__':
    app.run(debug=True)
