var Mspiff = (function () {

  function getSidFrom(node) {
    return parseInt( node.attr('id').substring( 'screening-'.length ))
  }

  function nodeForSid(sid) {
    return $(sid);
  }

  function renderDayTimeline(count,id,json) {
    var data = JSON.parse(json)
    var node = document.createElement( 'div' )

    node.setAttribute('id', 'day-timeline-' + id)

    var timeline =
       new vis.Timeline( node
                         , new vis.DataSet(data.items)
                         , new vis.DataSet(data.groups)
                         , data.options
                       )
    if ( id +1 == count ) {
      timeline.on('changed', function () {
        console.log( "invoke redraw" )
        redraw()
      })
    }
    var schedule = document.getElementById( 'schedule' )
    schedule.appendChild(node)
  }

  function resetPin() {
    $(this).removeClass('fa-circle')
    $(this).addClass('fa-circle-o')
  }

  function setEventHandlers() {

    $('#schedule').
      on( 'click', '.vis-item-content', {},
          function () {
            if ( !$(this).hasClass('control') ) {
              addScreening( getSidFrom( $('.screening',this) ) )
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
              $(this).attr( 'title', 'Pin Screening' )
              $(this).toggleClass( 'unpinned pinned' )
              $(this).find('.fa').toggleClass( 'fa-circle-o fa-circle')
              pinScreening( sid )
            } else if ( $(this).hasClass( 'pinned' ) ) {
              $(this).attr( 'title', 'Unpin Screening' )
              $(this).toggleClass( 'pinned unpinned' )
              $(this).find('.fa').toggleClass( 'fa-circle-o fa-circle')
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
            $('.vis-item-overflow').css('background-color','rgb(213,221,246)')
            $('.screening').find('.control').css('visibility','hidden')
            $('.pin-screening > a > .fa-circle').each( resetPin )
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

  function showControlsFor(sid) {
    $(sid).find('.control').css('visibility','visible')
  }

  function hideControlsFor(sid) {
    $(sid).find('.control').css('visibility','hidden')
    $(sid).find('.pin-screening > a > .fa-circle').each( resetPin )
  }

  function showBlurbModal(img,text) {
    if ( img == "" || text == "" ) {
      console.log( "NO MODAL DATA" )
      return
    }
    $(".blurb-modal").text( text )
    $(".blurb-modal").modal('show')
  }

  function postInit() {
    $('.footer').css( 'visibility','visible' )
    $('.screening-info').hover( function () {
      showBlurb(getSidFrom($(this).parents('.screening')))
    })
  }

  function main () {
    h$main(h$mainZCMainzimain);
  }

  return { main : main
           , renderDayTimeline : renderDayTimeline
           , setEventHandlers : setEventHandlers
           , getCookie : getCookie
           , setCookie : setCookie
           , removeCookie : removeCookie
           , showControlsFor : showControlsFor
           , hideControlsFor : hideControlsFor
           , showBlurbModal : showBlurbModal
           , postInit : postInit
         }
})()
$(document).ready( Mspiff.main )

