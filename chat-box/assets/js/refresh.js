function changeView(data){
      $("#papapa").html(data);
}
function update(){
 jQuery.ajax("/jquery.ajax",{
    success: changeView
 });
}

setTimeout(update, 1000);
