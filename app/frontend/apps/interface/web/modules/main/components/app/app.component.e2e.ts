declare var browser: any;

describe('AppComponent', () => {

  beforeEach(() => {
    browser.get('/');
  });


  it('should have a title', () => {
    let subject = browser.getTitle();
    let result  = 'Hello PeapDemo!';
    expect(subject).toEqual(result);
  });

});
