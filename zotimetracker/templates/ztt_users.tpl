            <div id="row">
                <div id="tracker_left"><b>User [{{ number_of_users }}]</b>
                </div>
                <div id="tracker_middle"><b>Activities</b>
                </div>
                <div id="tracker_right"></div>
            </div>
{% for username,timelines in users %}
            <div id="row">
                <div id="tracker_left">{{ username }}
                </div>
                <div id="tracker_middle_timeline">
                    {% for activity in timelines %}
                        <div title="{{activity["name"]}}" style="margin-top: -15px; position: absolute ; border:solid #939393 1px;opacity:0.6; background: {{activity["color"]}}; margin-left: {{activity["from"]}}px; width: {{activity["duration"]}}px;" >&nbsp;</div>
                    {% endfor %}
                </div>
                <div id="tracker_right"></div>
            </div>
{% endfor %}

