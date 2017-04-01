var Mspiff = (function () {

  function renderDay(day) {
    var data = document.getElementById("day-data-" + day)
    var venueCount = data.getAttribute("data-venue-count")
    var items = []
    var venues = []
    for (venueIndex = 0; venueIndex < venueCount; venueIndex++) {
      var venue = data.children[venueIndex]
      var screenings = venue.children
      for (screeningIndex = 0;
           screeningIndex < screenings.length;
           screeningIndex++) {
        var screening = screenings[screeningIndex]
        var startDate = new Date(screening.getAttribute( "data-start" ))
        var endDate = new Date(screening.getAttribute( "data-end" ))
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
          , moveable: true
          , showCurrentTime: false
        }
    var timeline = new vis.Timeline( node
                                     , new vis.DataSet(items)
                                     , new vis.DataSet(venues)
                                     , options
                                   )
  }

  renderTimeline = function () {
    console.log( "RENDER TIMELINE" )
    var data = document.getElementById("schedule-data")
    var dayCount = data.getAttribute("data-day-count")
    for (day = 1; day <= dayCount; day++) {
      console.log( "RENDER DAY " + day );
      renderDay(day);
    }
  }

  function init () {
    //var tid = h$main(h$mainZCZCMainzimain)
    console.log( "BOOTED: " + tid )
  }

  return { init : init
           , renderTimeline : renderTimeline
         }
})()

