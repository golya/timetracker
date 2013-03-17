<!DOCTYPE html>
<html lang="{{ z_language|default:"en"|escape }}">
<head>
    <meta charset="utf-8" />
    <title>Timetracker</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <meta name="author" content="Ádám Gólya" />
    {% lib "css/default.css" %}
    {% all include "_html_head.tpl" %}

    {% lib
            "css/jquery.loadmask.css"
    %}

    {% block html_head_extra %}{% endblock %}
</head>

<body>
<div class="container">
    <div class="content" style="margin-top: 30px">
        <h1>TimeTracker Demo</h1>
        <div id="table_container">
            <div id="row">
                <div id="left">
                    <div id="description">
                        This demo site only for practising task with zotonic.
                        With first form you can add new user to list. With the
                        second form you can add new activity to appropriate user.
                        In the timeline one pixel represent two minute.
                    </div>

                </div>
                <div id="right">
                    {% wire type="submit" id="add_user" postback="add_user" delegate="controller_zotimetracker" %}
                    <form id="add_user" method="post" action="postback">
                        <div class="clearfix" >
                        <b>Simulate user login</b><br/>
                        <input id="username" name="username" value="name" />
                        <br/>
                        <button class="button" >Add user</button>
                        </div>
                    </form><br/>
                    {% wire type="submit" id="add_activity" postback="add_activity" delegate="controller_zotimetracker" %}
                    <form id="add_activity" method="post" action="postback">
                        <div class="clearfix" >
                        <b>Simulate user activity</b><br/>
                        <input id="name" name="name" value="name" /><br/>
                        <b>Activity</b><br/>
                        <input id="activity" name="activity" value="activity" /><br/>
                        <b>From (time)</b><br/>
                        <input id="from" name="from" value="06:30" /><br/>
                        <b>Duration (in minutes)</b><br/>
                        <input id="duration" name="duration" value="450" />
                        {% validate id="duration" type={numericality minimum=0 maximum=1440} %}
                        <br/><button class="button" >Add activity</button>
                        </div>
                    </form>
                </div>

            </div>
        </div>
        <div id="users_container">
            {% all include "ztt_users.tpl" %}
        </div>
    </div>
</div>

{% include "_js_include_jquery.tpl" %}
{% lib
	"js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"
	"js/modules/livevalidation-1.3.js"
	"js/modules/z.inputoverlay.js"
%}

{% stream %}
{% script %}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
	});
</script>

</body>
</html>