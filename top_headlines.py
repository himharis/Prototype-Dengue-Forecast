import secrets
from flask import Flask, render_template 
from flask import Flask, redirect, url_for
import requests
import json
from flask import send_file



app = Flask(__name__)
@app.route('/')
def index():
    return render_template('homepage.html')

@app.route('/name/<nm>')
def hello_name(nm):
    return render_template('name.html', name=nm)

@app.route('/headlines/<nm>')
def headlines(nm):
    API_KEY = api.API_KEY
    BASE_URL = 'https://api.nytimes.com/svc/topstories/v2/{}.json?api-key={}'.format("technology", API_KEY)
    Response = requests.get(BASE_URL)
    top_stories = Response.json()

    title = []
    links = []
    thumbnails = []

    for i in top_stories['results']:
        if i['section'] == 'technology':
            title.append(i['title'])
            links.append(i['url'])
        for j in i['multimedia']:
            thumbnails.append(j['url'])
        else:
            pass
    data = {}
    for j in range(5):
        temp = []
        temp.append(title[j])
        temp.append(links[j])
        temp.append(thumbnails[j])
        data[j] = temp

    return render_template('j.html', name= nm, data=data)



if __name__ == '__main__':
    
    app.run(debug=True)



