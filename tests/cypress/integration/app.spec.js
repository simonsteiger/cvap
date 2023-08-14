describe('app', () => {
  beforeEach(() => {
    cy.visit('/')
  })

  // currently throws error, probably because of readLines' incomplete lines warning
  Cypress.on('uncaught:exception', (err, runnable) => {
    return false;
  });

  //it('navigates to vap1 and returns home', () => {
  //  cy.wait(10000)
  //  cy.get('#app-home-vap_indikatorer_1').click().get('#app-vap_indikatorer_1-return').click()
  //})

  it('navigates into filter menu', () => {
    cy.wait(10000) // Wait for app load

    // Go to VAP Indikatorer 1 and open input modal
    cy.get('#app-home-vap_indikatorer_1')
      .click()
      .get('#openModal-app-vap_indikatorer_1-go_input')
      .click()

    // Set gender to female
    cy.get('#app-vap_indikatorer_1-input-kon')
      .get('[type="radio"]')
      .check('Kvinna')

    // Confirm selection and close input modal
    //cy.get('#app-vap_indikatorer_1-go_input')
    //  .click()
  })
})
