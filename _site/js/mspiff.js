var Mspiff = (function () {

  function renderDayTimeline(id,json) {
    var data = JSON.parse(json)
    var node = document.createElement( 'div' )

    node.setAttribute( "id", "day-timeline-" + id)

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
      console.log("C: " + sid)
      eventCallback(sid)
    }

    $('#schedule').on( 'click', '.vis-item-content', {}, handler );
  }

  function init () {
    var tid = h$main(h$mainZCMainzimain);
    console.log( "TID: " + tid )
  }

  return { init : init
           , renderDayTimeline : renderDayTimeline
           , setEventHandler : setEventHandler
         }
})()
$(document).ready( Mspiff.init )

