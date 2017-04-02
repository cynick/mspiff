var Mspiff = (function () {

  function renderDayTimeline(day) {
    console.log( "RENDER DAY " + day );
    var data = document.getElementById("day-data-" + day)
    var venueCount = data.getAttribute("data-venue-count")
    var items = []
    var venues = []
    var minTs = null;
    var maxTs = null;

    for (venueIndex = 0; venueIndex < venueCount; venueIndex++) {
      var venue = data.children[venueIndex]
      var screenings = venue.children
      for (screeningIndex = 0;
           screeningIndex < screenings.length;
           screeningIndex++) {
        var screening = screenings[screeningIndex]
        var startDate = new Date(screening.getAttribute( "data-start"))
        var endDate = new Date(screening.getAttribute( "data-end"))
        if ( minTs === null || startDate < minTs ) {
          minTs = startDate;
        }
        if ( maxTs === null || endDate > maxTs ) {
          maxTs = endDate;
        }

        var item = { id: screening.getAttribute("id")
                     , start: startDate
                     , end: endDate
                     , content: screening
                     , group: venueIndex
                     , title: screening.getElementByClass
                   }
        items.push(item)
      }
      venues.push({ id: venueIndex
                    , content: venue.getAttribute("data-venue-name")
                  })
    }

    var node = document.getElementById( "day-timeline-" + day)
    var options =
        { zoomable: false
          , moveable: false
          , showCurrentTime: false
          , min: minTs
          , max: maxTs
        }
    var timeline = new vis.Timeline( node
                                     , new vis.DataSet(items)
                                     , new vis.DataSet(venues)
                                     , options
                                   )
  }

  function renderDayTimeline_(data) {
    var day = data.day
    var venueCount = data.venues.length
    var items = []
    var venues = []
    var minTs = null;
    var maxTs = null;

    for (venueIndex = 0; venueIndex < venueCount; venueIndex++) {
      var venue = data.venues[venueIndex]
      var screenings = venue.screenings
      for (screeningIndex = 0;
           screeningIndex < screenings.length;
           screeningIndex++) {
        var screening = screenings[screeningIndex]
        var start = new Date(screening.start)
        var end = new Date(screening.end)
        if ( minTs === null || startDate < minTs ) {
          minTs = startDate;
        }
        if ( maxTs === null || endDate > maxTs ) {
          maxTs = endDate;
        }

        var item = { id: screening.id
                     , start: start
                     , end: end
                     , content: screening.html
                     , group: venueIndex
                     , title: screening.title
                   }
        items.push(item)
      }

      venues.push({ id: venueIndex
                    , content: venue.name
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

