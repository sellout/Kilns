var rgraph;

window.requestAnimFrame = (function(callback){
                               return window.requestAnimationFrame ||
                                   window.webkitRequestAnimationFrame ||
                                   window.mozRequestAnimationFrame ||
                                   window.oRequestAnimationFrame ||
                                   window.msRequestAnimationFrame ||
                                   function(callback){
                                       window.setTimeout(callback, 1000 / 60);
                                   };
                           })();
//      var source = new EventSource("/current-kells"); 

var TAU = 2 * Math.PI;

function draw_kell (context, label, x, y) {
    context.fillStyle="#0000ff";
    context.beginPath();
    context.arc(x, y, 30, 0, TAU, false);
    context.closePath();
    context.fill();
    context.fillStyle="#000000";
    context.fillText(label, x, y);
}


var order = 1;

function draw_kells (kell, rank, context) {
    draw_kell(context, kell.id, order * 70, rank * 70);
    if (kell.children) {
        kell.children.forEach(function (k) { 
                                  draw_kells(k, rank+1, context);
                              });
    }
    order += 1;
}

var firstTime = true;

function animate () {
var xmlhttp = new XMLHttpRequest();  
xmlhttp.onreadystatechange = function() {
    if(xmlhttp.readyState == 4){
        if (xmlhttp.status === 200) {  
            var hierarchy = JSON.parse(xmlhttp.responseText);
            if (firstTime) {
                rgraph.loadJSON(hierarchy);
                firstTime = false;
            } else {
                rgraph.op.sum(hierarchy,
                              { type: 'fade:con',
                                hideLabels: false,
                                duration: 1000
                              });
            }  
            // var canvas = document.getElementById('kells');
            // // Check the element is in the DOM and the browser supports canvas
            // if(canvas.getContext) {
            //     var context = canvas.getContext('2d');
            //     context.clearRect(0, 0, canvas.width, canvas.height);
            //     order = 1;
            //     draw_kells(hierarchy, 1, context);
            // }
        }
        requestAnimFrame(function(){
                             animate();
                         });        
    }
};
    xmlhttp.open('GET', '/current-kells', true);  
    xmlhttp.send();
}

//      source.onmessage = function(event) {
//        var drawingCanvas = document.getElementById('kells');
        // Check the element is in the DOM and the browser supports canvas
//        if(drawingCanvas.getContext) {
//          var context = drawingCanvas.getContext('2d');
//          var hierarchy = eval('(' + event.data + ')');
//          draw_kells(hierarchy, 0, context);
//        }
//      };

function init () {
    rgraph = new $jit.RGraph({ 'injectInto': 'kells', 
                               width: 1000,
                               height: 600,
                               levelDistance: 50,
                               Navigation: {  
                                   enable: true,  
                                   panning: true,  
                                   zooming: 10  
                               },  
                               Node: {  
                                   color: '#009999',  
                                   overridable:true  
                               },  
                               Edge: {  
                                   overridable:true,  
                                   color: '#999999',
                                   type: 'arrow'
                               },  
                               onCreateLabel: function(domElement, node){  
                                   domElement.innerHTML = node.name;
                                   domElement.onclick = function(){  
                                       rgraph.onClick(node.id, {  
                                                          onComplete: function() {  
                                                              Log.write("done");
                                                          }  
                                                      });  
                                   };
                               },
                               onBeforePlotNode: function(node) {
                                   node.data.$dim = (Math.sqrt(node.data.size/Math.PI) + 1);
                                   if (node.data.active) {
                                       node.data.$color = '#ff0000';
                                   } else {
                                       node.data.$color = '#009999';
                                      }
                                   if (node.data.repl) {
                                       node.data.$type = 'star';
                                       // need it to be big enough
                                       // to see
                                       node.data.$dim = Math.max(6, node.data.$dim);
                                   } else {
                                       node.data.$type = 'circle';
                                   }
                               }
                             });
    animate();
}

window.onload = function(){
    animate();
};
