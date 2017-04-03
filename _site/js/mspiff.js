var Mspiff = (function () {

  function renderDayTimeline_(catalog,day) {
    var data = catalog.days[day]
    var venueCount = data.venues.length
    var items = []
    var venues = []
    var minTs = null;
    var maxTs = null;

    for (venueIndex = 0; venueIndex < venueCount; venueIndex++) {
      var venue = data.venues[venueIndex]
      var events = venue.events
      for (eventIndex = 0;
           eventIndex < events.length;
           eventIndex++) {
        var event = events[eventIndex]
        var screening = catalog.screenings[event.id]
        var start = new Date(screening.start)
        var end = new Date(start.getTime() + screening.duration)
        if ( minTs === null || startDate < minTs ) {
          minTs = startDate;
        }
        if ( maxTs === null || endDate > maxTs ) {
          maxTs = endDate;
        }

        var item = { id: screening.filmId
                     , start: start
                     , end: end
                     , content: screening.html
                     , group: venueIndex
                     , title: catalog.films[ screening.filmId ].filmName
                   }
        items.push(item)
      }

      venues.push({ id: venueIndex
                    , content: catalog.venues[ screening.venueId ].name
                  })
    }

    var node = document.createElement( 'div' )
    node.setAttribute( "id", "day-timeline-" + day)
    var options =
        { zoomable: false
          , moveable: false
          , showCurrentTime: false
          , min: minTs
          , max: maxTs
        }
    var timeline =
       new vis.Timeline( node
                         , new vis.DataSet(items)
                         , new vis.DataSet(venues)
                         , options
                       )
    document.body.appendChild(node)
  }

  function renderDayTimeline__(id,json) {
    var data = JSON.parse(json)
    var node = document.createElement( 'div' )

    node.setAttribute( "id", "day-timeline-" + id)

    var timeline =
       new vis.Timeline( node
                         , new vis.DataSet(data.items)
                         , new vis.DataSet(data.venues)
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

