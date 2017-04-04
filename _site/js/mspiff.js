var Mspiff = (function () {

  function renderDayTimeline(id,json) {
    var data = JSON.parse(json)
    var node = document.createElement( 'div' )

    node.setAttribute("id", "day-timeline-" + id)

    var timeline =
       new vis.Timeline( node
                         , new vis.DataSet(data.items)
                         , new vis.DataSet(data.groups)
                         , data.options
                       )
    var schedule = document.getElementById( "schedule" );
    schedule.appendChild(node)
  }

  function setEventHandler () {
    handler = function () {
      var s = $(".screening",this)
      var sid = s.attr('id').substring( "screening-".length )
      eventCallback(parseInt(sid))
    }

    $('#schedule').on( 'click', '.vis-item-content', {}, handler );
  }

  function setCookie(json) {
    Cookie.set("data",JSON.parse(json))
  }

  function getCookie() {
    return Cookie.get("data")
  }

  function init () {
    h$main(h$mainZCMainzimain);
  }

  return { init : init
           , renderDayTimeline : renderDayTimeline
           , setEventHandler : setEventHandler
           , getCookie
           , setCookie
         }
})()
$(document).ready( Mspiff.init )

