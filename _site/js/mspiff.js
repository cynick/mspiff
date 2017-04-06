var Mspiff = (function () {

  function renderDayTimeline(id,json) {
    var data = JSON.parse(json)
    var node = document.createElement( 'div' )

    node.setAttribute('id', 'day-timeline-' + id)

    var timeline =
       new vis.Timeline( node
                         , new vis.DataSet(data.items)
                         , new vis.DataSet(data.groups)
                         , data.options
                       )
    var schedule = document.getElementById( 'schedule' )
    schedule.appendChild(node)
  }

  function setEventHandlers() {

    function getSidFrom(node) {
      return parseInt( node.attr('id').substring( 'screening-'.length ))
    }

    $('#schedule').
      on( 'click', '.vis-item-content', {},
          function () {
            if ( !$(this).hasClass('control') ) {
              addScreening( getSidFrom( $('.screening',this) ) )
              $(this).find('.control').css('visibility','visible')
            }
          }
        )
    $('#schedule').
      on( 'click', '.ruleout-screening', {},
          function () {
            ruleOutScreening(getSidFrom($(this).parents('.screening')))
            return false;
          }
        )
    $('#schedule').
      on( 'click', '.pin-screening', {},
          function () {
            var sid = getSidFrom($(this).parents('.screening'))
            if ($(this).hasClass( 'unpinned')) {
              $(this).toggleClass( 'unpinned pinned' )
              $(this).find('.fa').toggleClass( 'fa-circle-o fa-circle')
              pinScreening( sid )
              console.log( "PIN" )
            } else if ( $(this).hasClass( 'pinned' ) ) {
              $(this).toggleClass( 'pinned unpinned' )
              $(this).find('.fa').toggleClass( 'fa-circle-o fa-circle')
              console.log( "UNPIN" )
              unPinScreening( sid )
            }
            return false;
          }
        )
    $('#schedule').
      on( 'click', '.remove-film', {},
          function () {
            var screening = $(this).parents('.screening')
            removeFilm( getSidFrom(screening) )
            screening.find('.control').css('visibility','hidden')
            return false;
          }
        )
    $('.clear-all').
      on( 'click', "", {},
          function () {
            removeCookie()
            clearState()
            return false;
          }
        )
  }

  function setCookie(json) {
    Cookies.set('data',JSON.parse(json))
  }

  function getCookie() {
    return Cookies.get('data') || ""
  }

  function removeCookie() {
    return Cookies.remove('data')
  }

  function postInit() {
    $('.footer').css( 'visibility','visible' )
  }

  function main () {
    h$main(h$mainZCMainzimain);
  }

  return { main : main
           , renderDayTimeline : renderDayTimeline
           , setEventHandlers : setEventHandlers
           , getCookie : getCookie
           , setCookie : getCookie
           , removeCookie : removeCookie
           , postInit : postInit
         }
})()
$(document).ready( Mspiff.main )

