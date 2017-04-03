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
    document.body.appendChild(node)
  }

  function turnOffSpinner () {
    console.log( "TURN OFF SPINNER" )
    $("#loading").css('visibility','hidden')
  }

  function init () {
    var tid = h$main(h$mainZCMainzimain);
    console.log( "TID: " + tid )
  }

  return { init : init
           , turnOffSpinner : turnOffSpinner
           , renderDayTimeline : renderDayTimeline
         }
})()
$(document).ready( Mspiff.init )

