


$(document).ready(function(){
    $('li.paper .Abstract').click(function() {
        var text = $(this).parent().siblings('.resumecontent').eq(0);
        if(text.is(':hidden')){
            text.slideDown('500');
            $(this).parent().siblings('.bidule').eq(0).html('-');
        }
        else {
            text.slideUp('300');
            $(this).parent().siblings('.bidule').eq(0).html('+');
        }
    });
    $('li.paper .bidule').click(function() {
        var text = $(this).siblings('.resumecontent').eq(0);
        if(text.is(':hidden')){
            text.slideDown('500');
            $(this).html('-');
        }
        else {
            text.slideUp('300');
            $(this).html('+');
        }
    });
    $('li.paper .resumecontent').click(function() {
        $(this).slideUp('300');
        $(this).siblings('.bidule').eq(0).html('+');
    });
});

$(document).ready(function(){
    $('dl .bidule').click(function() {
        var text = $(this).parent().siblings('.moremenu').eq(0);
        if(text.is(':hidden')){
            text.slideDown('500');
            $(this).html('-');
        }
        else {
            text.slideUp('300');
            $(this).html('+');
        }
    });
});

$(document).ready(function(){
    $('#TMEEMTMenu li .bidule').click(function() {
        var text = $(this).siblings('.resumecontent').eq(0);
        if(text.is(':hidden')){
            text.slideDown('500');
            $(this).html('-');
        }
        else {
            text.slideUp('300');
            $(this).html('+');
        }
    });
    $('#TMEEMTMenu li .resumecontent').click(function() {
        $(this).slideUp('300');
        $(this).siblings('.bidule').eq(0).html('+');
    });
});
