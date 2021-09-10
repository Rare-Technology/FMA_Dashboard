function tour(){
  var Steps = [
    {
      element: '.sw-dropdown',
      intro: 'Click here to open the filters panel. You can select filters such as location, date, fish, and more. <strong>You must select at least one Managed Access Area to see results!</strong>',
      postion: 'bottom'
    },
    {
      element: "a[data-value='1. Assess data']",
      intro: 'After selecting your filters, you can see the data in a table here.',
      position: 'bottom'
    },
    {
      element: "a[data-value='2. Visualize data']",
      intro: 'Or you can see plots of the data here. You can also download the plots.',
      position: 'bottom'
    },
    {
      element: "a[data-value='3. Interpret results']",
      intro: "And here you will find a summary of the data which will help you with decision making.",
      position: 'bottom'
    },
    {
      intro: "That's the end of the tutorial! Any questions or concerns? Contact us at <a href='#' class='sneakymail' data-name='scitech' data-domain='rare' data-tld='org' onclick=\"window.location.href = 'mailto:' + this.dataset.name + '@' + this.dataset.domain + '.' + this.dataset.tld; return false;\"></a>"
    }
  ];
  
  // initialize introjs instance
  var intro = introJs();
  
  // load the steps into the tour
  intro.setOptions({steps: Steps});
  
  intro.start();
};
